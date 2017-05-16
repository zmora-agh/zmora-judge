{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module RabbitMQ
  ( startRabbitMQWorker
  ) where

import           Configuration
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy        as B
import           Data.Maybe                  (maybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Network.AMQP                hiding (consumeMsgs)
import           Network.AMQP.Lifted
import           Zmora.AMQP

type RabbitMsg = (Message, Envelope)

type RabbitResponseFor = (Either RawTask RawTaskResult, Envelope)

type RawTask = B.ByteString

type RawTaskResult = B.ByteString

addConnectionClosedLiftedHandler
  :: MonadBaseControl IO m
  => Connection -> m a -> m ()
addConnectionClosedLiftedHandler connection action =
  liftBaseWith $ \runInIO ->
    addConnectionClosedHandler connection True $ void . runInIO $ action

addChannelExceptionLiftedHandler
  :: MonadBaseControl IO m
  => Channel -> (SomeException -> m a) -> m ()
addChannelExceptionLiftedHandler channel action =
  liftBaseWith $ \runInIO ->
    addChannelExceptionHandler channel $ void . runInIO . action

startRabbitMQWorker
  :: (MonadBaseControl IO m, MonadLoggerIO m)
  => Maybe String -> (B.ByteString -> IO B.ByteString) -> m ()
startRabbitMQWorker uri executor = do
  $(logInfo) $
    "Broker URI: " <>
    maybe
      "default"
      (\u -> (T.pack . show) u <> " (defaults used for missing parameters)")
      uri
  let connOpts = maybe rabbitMQConnectionOpts fromURI uri

  connection <- catch (liftIO $ openConnection'' connOpts) handleConnectEx
  addConnectionClosedLiftedHandler connection handleConnectionClosed

  channel <- liftIO $ do
    channel <- openChannel connection
    qos channel 0 1 False
    declareStandardQueues channel
    return channel

  addChannelExceptionLiftedHandler channel handleChannelEx

  responseQueue <- liftIO $ atomically newTQueue
  _ <- consumeMsgs channel taskQueueName Ack $ processMsg responseQueue executor

  forever $ do
    response <- liftIO $ atomically $ readTQueue responseQueue
    processResponse channel response

  where
    handleConnectEx :: MonadLoggerIO m => AMQPException -> m Connection
    handleConnectEx e = logErrorException "AMQP connect failed." e >>= throw
    handleChannelEx = logErrorException "Channel closed."
    handleConnectionClosed = $(logError) "AMQP connection closed."
    logErrorException msg e = do
      $(logError) $ msg <> " Exception: " <> (T.pack . displayException) e
      return e

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
  liftIO $ atomically $ writeTQueue queue $! (result, env)
