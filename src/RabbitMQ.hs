{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ExistentialQuantification #-}

module RabbitMQ
  ( startRabbitMQWorker
  ) where

import           Control.Concurrent          (myThreadId)
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Lens                ((^.), view)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy        as B
import           Data.Foldable               (toList)
import           Data.Maybe                  (fromJust)
import           Data.Monoid                 ((<>))
import           Data.List                   (intercalate)
import qualified Data.Text                   as T
import           Network.AMQP                hiding (consumeMsgs)
import           Network.AMQP.Lifted
import           AMQP
import qualified QueueModel                  as M
import           Text.ProtocolBuffers        (messagePut)

type RabbitMsg = (Message, Envelope)

type RabbitResponseFor = (Either RawTask RawTaskResult, Envelope)

type RawTask = B.ByteString

type RawTaskResult = B.ByteString

type ExecutorIn = M.Task
type ExecutorOut m = m M.TaskResult
type Executor m = ExecutorIn -> ExecutorOut m

rethrowChanExceptionsHere :: Channel -> IO ()
rethrowChanExceptionsHere chan = do
  thrId <- myThreadId
  addChannelExceptionHandler chan (throwTo thrId)

startRabbitMQWorker
  :: (MonadBaseControl IO m, MonadLoggerIO m)
  => T.Text -> Executor m -> m ()
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
processResponse channel (Right result, env) = liftIO $ do
  _ <- publishMsg channel "" taskResultQueueName newMsg {msgBody = result}
  ackEnv env
processResponse channel (Left task, env) = liftIO $ do
  _ <- publishMsg channel "" taskErrorQueueName newMsg {msgBody = task}
  rejectEnv env False

data ExecutorException = ExecutorException ExecutorIn SomeException
instance Show ExecutorException where
  show (ExecutorException task e) =
    "exception during #" <> tId <> " execution: " <> displayException e
    where
      tId = show . fromJust $ task ^. M.task_id

instance Exception ExecutorException

withAuditLog
  :: MonadLoggerIO m
  => Executor m -> ExecutorIn -> ExecutorOut m
withAuditLog executor task = do
  taskResult <- executor task

  let tIdStr = "#" <> (show . fromJust $ task ^. M.task_id)
  let inputLog = T.pack $ tIdStr <> " " <> showFileIds task
  let outputLog = T.pack $ showTestResults taskResult

  $(logDebug) $ "Executor audit: " <> inputLog <> " -> " <> outputLog

  return taskResult
  where
    showFileId = show . fromJust . view M.file_id
    showFileIds x =
      "(" <>
      intercalate
        ", "
        (map (\t -> "#" <> showFileId t) (toList $ x ^. M.files)) <>
      ")"
    showTestResult test =
      "(" <>
      intercalate ", " [
        "#" <> (show . fromJust $ test ^. M.source_test_id)
      ,  show . fromJust $ test ^. M.status
      ,  (show . fromJust $ test ^. M.user_time) <> "us"
      ,  (show . fromJust $ test ^. M.system_time) <> "us"
      ,  (show . fromJust $ test ^. M.ram_usage) <> "kB"
      ] <>
      ")"
    showTestResults x =
      intercalate ", " . map showTestResult . toList $ x ^. M.tests_results

wrapExceptions
  :: MonadBaseControl IO m
  => (M.Task -> m a) -> M.Task -> m a
wrapExceptions executor exInput =
  catch (executor exInput) (throw . ExecutorException exInput)

processMsg
  :: (MonadBaseControl IO m, MonadLoggerIO m)
  => TQueue RabbitResponseFor
  -> Executor m
  -> RabbitMsg
  -> m ()
processMsg queue executor (msg, env) = do
  executionIO <- try . (deserialize >=> runExecutor) $ rawTask
  response <- case executionIO of
    Left (e :: SomeException) -> do
      logException e
      return . Left $ rawTask
    Right result -> return . Right $ result

  enqueue response

  where
    deserialize = liftIO . M.messageGetIO
    serialize = messagePut
    logException e =
      $(logError) $ "Executor failed: " <> (T.pack . displayException $ e)
    runExecutor input = do
      $(logInfo) $ "Starting executor on task #" <>
        (T.pack . show . fromJust . view M.task_id) input
      withAuditLog (wrapExceptions executor) input
    enqueue result =
      liftIO . atomically . writeTQueue queue $ (serialize <$> result, env)
    rawTask = msgBody msg
