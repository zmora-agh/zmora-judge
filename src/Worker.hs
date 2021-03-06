{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Worker where

import           Compiler
import           Configuration               (executionTimeoutMult)
import           Control.Concurrent          (threadDelay)
import           Control.Exception.Lifted
import           Control.Monad               (forM, forever)
import           Control.Monad.Base          (liftBase)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Logger        (MonadLoggerIO, logError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Lens                ((^.))
import qualified Data.ByteString.Lazy        as B
import           Data.Foldable               (toList)
import           Data.Maybe                  (fromJust)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           GHC.Int                     (Int64)
import           Jail
import           RabbitMQ
import qualified Runnable                    as R
import           System.Directory            (setCurrentDirectory)
import           System.IO.Temp              (withSystemTempDirectory)
import           Text.ProtocolBuffers        (toString)
import qualified QueueModel                  as M

foreverDelayed :: MonadBaseControl IO m => Int -> m a -> m a
foreverDelayed seconds action =
  forever $ action >> (liftBase . threadDelay) (seconds * 1000000)

logWorkerException :: (MonadLoggerIO m, Exception e) => Int -> e -> m ()
logWorkerException delay e =
  $(logError) $ "RabbitMQ worker exception: " <> cause <>
  ". Retrying after " <> (T.pack . show) delay <> "s."
  where cause = T.pack . show $ e

startWorker :: (MonadLoggerIO m, MonadBaseControl IO m) => T.Text -> m ()
startWorker uri = foreverDelayed delay $
  startRabbitMQWorker uri (liftIO . processTask) `catches`
  [
    Handler (\(e :: AsyncException) -> throw e), -- rethrow UserInterrupt, etc.
    Handler (\(e :: SomeException) -> logWorkerException delay e)
  ]
  where delay = 30

processTask :: M.Task -> IO M.TaskResult
processTask task =
  withSystemTempDirectory "zmora-judge" $ \directory -> do
    setCurrentDirectory directory
    testsResults <- exampleProblemJudge taskFiles taskTests
    let result = M.taskResult (fromJust $ task ^. M.task_id) testsResults
    return result
      where taskFiles = toList $ task ^. M.files
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
        testCpuLimit = fromJust $ test ^. M.time_limit
        testMemLimit = fromJust $ test ^. M.ram_limit
        testTimeout = max 1 $
          executionTimeoutMult * fromIntegral testCpuLimit `div` 1000000

    (out, stats) <- run (R.Runner . Just $ testTimeout) "./a.out" [] testInput

    let status = getStatus stats (out == testOutput) testMemLimit testCpuLimit
    return $ M.testResult
      (fromJust $ test ^. M.test_id)
      status
      (R.userTime stats)
      (R.systemTime stats)
      (R.maxMemory stats)

getStatus :: R.RunnerOutput -> Bool -> Int64 -> Int64 -> M.Status
getStatus (R.RunnerOutput _ _ _ _ _ True) _ _ _ = M.TLE
getStatus (R.RunnerOutput _ _ _ _ False _) _ _ _ = M.RTE
getStatus (R.RunnerOutput _ _ _ _ _ _) False _ _ = M.ANS
getStatus (R.RunnerOutput _ mem _ _ _ _) _ memLimit _ | mem > memLimit = M.MEM
getStatus (R.RunnerOutput _ _ _ cpu _ _) _ _ cpuLimit | cpu > cpuLimit = M.TLE
getStatus (R.RunnerOutput _ _ _ _ _ _) True _ _ = M.OK
