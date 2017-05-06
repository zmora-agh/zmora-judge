{-# LANGUAGE OverloadedStrings #-}
module RabbitMQ ( startRabbitMQWorker ) where

import           Configuration
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy   as B
import           Data.Maybe             (maybe)
import           Network.AMQP
import           Zmora.AMQP

type RabbitMsg = (Message, Envelope)
type RabbitResponseFor = (Either RawTask RawTaskResult, Envelope)
type RawTask = B.ByteString
type RawTaskResult = B.ByteString

startRabbitMQWorker :: Maybe String -> (B.ByteString -> IO B.ByteString) -> IO ()
startRabbitMQWorker uri executor = do
  putStrLn $ "Broker URI: " ++
    maybe "default"
          (\u -> show u ++ " (defaults used for missing parameters)")
          uri
  let connOpts = maybe rabbitMQConnectionOpts fromURI uri
  connection <- openConnection'' connOpts

  channel <- openChannel connection
  qos channel 0 1 False
  declareStandardQueues channel

  responseQueue <- atomically newTQueue

  consumeMsgs channel taskQueueName Ack $
    processMsg responseQueue executor

  forever $ do
    response <- atomically $ readTQueue responseQueue
    processResponse channel response

  closeConnection connection

processResponse :: Channel -> RabbitResponseFor -> IO ()
processResponse channel (Right result, env) = do
    publishMsg channel "" taskResultQueueName newMsg {msgBody = result}
    ackEnv env
processResponse channel (Left task, env) = do
    publishMsg channel "" taskErrorQueueName newMsg {msgBody = task}
    rejectEnv env False

processMsg :: TQueue RabbitResponseFor -> (RawTask -> IO RawTaskResult) -> RabbitMsg -> IO ()
processMsg queue executor (msg, env) = do
  let body = msgBody msg
  result <- catch (Right <$> executor body) $ \e -> do
    print (e :: SomeException)
    return $ Left body
  atomically $ writeTQueue queue $! (result, env)

