{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Worker where

import           Compiler
import           Control.Concurrent          (threadDelay)
import           Control.Exception.Lifted
import           Control.Monad               (forM, forever)
import           Control.Monad.Base          (MonadBase, liftBase)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Logger        (MonadLogger, MonadLoggerIO,
                                              logError)
import           Control.Lens                ((^.))
import qualified Data.ByteString.Lazy        as B
import           Data.Foldable               (toList)
import           Data.Maybe                  (fromJust)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Jail
import           RabbitMQ
import qualified Runnable                    as R
import           System.Directory
import           System.IO.Temp
import           Text.ProtocolBuffers        (toString, messageGet, messagePut)
import qualified QueueModel                  as M

foreverDelayed :: MonadBase IO m => Int -> m a -> m a
foreverDelayed seconds action =
  forever $ action >> (liftBase . threadDelay) (seconds * 1000000)

logWorkerException :: (MonadLogger m, Exception e) => Int -> e -> m ()
logWorkerException delay e =
  $(logError) $ "RabbitMQ worker exception: " <> cause <>
  ". Retrying after " <> (T.pack . show) delay <> "s."
  where cause = T.pack . show $ e

startWorker :: (MonadLoggerIO m, MonadBaseControl IO m) => T.Text -> m ()
startWorker uri = foreverDelayed delay $
  startRabbitMQWorker uri processTask `catches`
  [
    Handler (\(e :: AsyncException) -> throw e), -- rethrow UserInterrupt, etc.
    Handler (\(e :: SomeException) -> logWorkerException delay e)
  ]
  where delay = 30

processTask :: B.ByteString -> IO B.ByteString
processTask rawTask =
  withSystemTempDirectory "zmora-judge" $ \directory -> do
      setCurrentDirectory directory
      testsResults <- exampleProblemJudge taskFiles taskTests
      let result = M.taskResult taskId testsResults
      return $ messagePut result
  where
    task = either error fst (messageGet rawTask)
    taskId = fromJust $ task ^. M.task_id
    taskFiles = toList $ task ^. M.files
    taskTests = toList $ task ^. M.tests

save :: M.File -> IO ()
save taskFile = B.writeFile filename bytes
  where
    filename = toString . fromJust $ taskFile ^. M.name
    bytes = fromJust $ taskFile ^. M.content

saveAll :: [M.File] -> IO ()
saveAll = mapM_ save

exampleProblemJudge :: [M.File] -> [M.Test] -> IO [M.TestResult]
exampleProblemJudge files tests = do
  saveAll files

  let filenames = (\file -> toString . fromJust $ file ^. M.name) <$> files
  _ <- withCompiler (defaultPreset :: GCC) $ compile filenames "a.out"

  --TODO redo interface for running test
  forM tests $ \test -> withJail $ do
    let testInput  = toString . fromJust $ test ^. M.input
        testOutput = toString . fromJust $ test ^. M.output

    (out, stats) <- run R.Runner "./a.out" [] testInput

    status <- if out == testOutput then return M.OK else return M.ANS
    return $ M.testResult
      (fromJust $ test ^. M.test_id)
      status
      (round . R.userTime $ stats)
      (R.maxMemory stats)
