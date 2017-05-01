{-# LANGUAGE OverloadedStrings #-}
module RabbitMQ ( startRabbitMQWorker ) where

import           Configuration
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text              as T
import           Network.AMQP

tasksQueueName = "tasks"
tasksErrorsQueueName = "tasksErrors"
tasksResultsQueueName = "tasksResults"

type RabbitMsg = (Message, Envelope)
type RabbitResponseFor = (Either RawTask RawTaskResult, Envelope)
type RawTask = B.ByteString
type RawTaskResult = B.ByteString

startRabbitMQWorker :: (B.ByteString -> IO B.ByteString) -> IO ()
startRabbitMQWorker executor = do
  connection <- openConnection'' rabbitMQConnectionOpts

  channel <- openChannel connection
  qos channel 0 1 False
  declareQueues channel [tasksQueueName, tasksResultsQueueName, tasksErrorsQueueName]

  responseQueue <- atomically $ newTQueue

  consumeMsgs channel tasksQueueName Ack $
    processMsg responseQueue executor

  forever $ do
    response <- atomically $ readTQueue responseQueue
    processResponse channel response

  putStrLn "Press any key to exit."
  getLine
  closeConnection connection

declareQueues :: Channel -> [T.Text] -> IO ()
declareQueues channel = mapM_ (\n -> declareQueue channel newQueue{queueName = n})

processResponse :: Channel -> RabbitResponseFor -> IO ()
processResponse channel (Right result, env) = do
    publishMsg channel "" tasksResultsQueueName newMsg {msgBody = result}
    ackEnv env
processResponse channel (Left task, env) = do
    publishMsg channel "" tasksErrorsQueueName newMsg {msgBody = task}
    rejectEnv env False

processMsg :: TQueue RabbitResponseFor -> (RawTask -> IO RawTaskResult) -> RabbitMsg -> IO ()
processMsg queue executor (msg, env) = do
  let body = msgBody msg
  result <- catch (Right <$> executor body) $ \e -> do
    print (e :: SomeException)
    return $ Left body
  atomically $ writeTQueue queue $! (result, env)

