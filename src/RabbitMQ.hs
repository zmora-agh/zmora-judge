{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module RabbitMQ
  ( startRabbitMQWorker
  ) where

import           Control.Concurrent          (myThreadId)
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy        as B
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Network.AMQP                hiding (consumeMsgs)
import           Network.AMQP.Lifted
import           AMQP

type RabbitMsg = (Message, Envelope)

type RabbitResponseFor = (Either RawTask RawTaskResult, Envelope)

type RawTask = B.ByteString

type RawTaskResult = B.ByteString

rethrowChanExceptionsHere :: Channel -> IO ()
rethrowChanExceptionsHere chan = do
  thrId <- myThreadId
  addChannelExceptionHandler chan (throwTo thrId)

startRabbitMQWorker
  :: (MonadBaseControl IO m, MonadLoggerIO m)
  => T.Text -> (B.ByteString -> IO B.ByteString) -> m ()
startRabbitMQWorker uri executor = do
  $(logInfo) $ "Trying to connect RabbitMQ broker using URI: " <> uri
  let connOpts = fromURI . T.unpack $ uri

  connection <- liftIO $ openConnection'' connOpts

  channel <- liftIO $ do
    channel <- openChannel connection
    qos channel 0 1 False
    declareStandardQueues channel
    rethrowChanExceptionsHere channel
    return channel

  responseQueue <- liftIO $ atomically newTQueue
  _ <- consumeMsgs channel taskQueueName Ack $ processMsg responseQueue executor

  $(logInfo) "Connected, waiting for tasks..."
  forever $ do
    response <- liftIO $ atomically $ readTQueue responseQueue
    processResponse channel response

processResponse
  :: (MonadLoggerIO m)
  => Channel -> RabbitResponseFor -> m ()
processResponse channel (Right result, env) = do
  $(logInfo) "Task successfully processed"
  _ <-
    liftIO $ publishMsg channel "" taskResultQueueName newMsg {msgBody = result}
  liftIO $ ackEnv env
processResponse channel (Left task, env) = do
  $(logError) "Task execution failed"
  _ <- liftIO $ publishMsg channel "" taskErrorQueueName newMsg {msgBody = task}
  liftIO $ rejectEnv env False

processMsg
  :: (MonadLoggerIO m)
  => TQueue RabbitResponseFor
  -> (RawTask -> IO RawTaskResult)
  -> RabbitMsg
  -> m ()
processMsg queue executor (msg, env) = do
  $(logInfo) "Received new message"
  let body = msgBody msg
  result <-
    liftIO $
    catch (Right <$> executor body) $ \e -> do
      print (e :: SomeException)
      return $ Left body
  liftIO $ atomically $ writeTQueue queue (result, env)
