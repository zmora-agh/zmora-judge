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
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.Maybe
import           Data.Monoid                 ((<>))
import           Data.ProtocolBuffers
import qualified Data.Text                   as T
import           Jail
import           RabbitMQ
import qualified Runnable                    as R
import           System.Directory
import           System.IO.Temp
import           Zmora.Queue

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

processTask :: BL.ByteString -> IO BL.ByteString
processTask rawTask = either error process (deserialize rawTask)
  where process task = withSystemTempDirectory "zmora-judge" $ \directory -> do
          setCurrentDirectory directory
          testsResults <- exampleProblemJudge taskFiles taskTests
          let result = TaskResult
                { resultId       = taskId task
                , compilationLog = putField . Just $ ""
                , testResults    = putField testsResults
                }
          return $ serialize result
            where taskFiles = getField . files $ task
                  taskTests = getField . tests $ task

save :: File -> IO ()
save file = BS.writeFile filename bytes
  where filename = T.unpack . fromJust . getField . name $ file
        bytes = fromJust . getField . content $ file

saveAll :: [File] -> IO ()
saveAll = mapM_ save

exampleProblemJudge :: [File] -> [Test] -> IO [TestResult]
exampleProblemJudge files tests = do
  saveAll files

  let filenames = T.unpack . fromJust . getField . name <$> files
  _ <- withCompiler (defaultPreset :: GCC) $ compile filenames "a.out"

  --TODO redo interface for running test
  forM tests $ \test -> withJail $ do
    let testInput  = T.unpack . fromJust . getField . input $ test
        testOutput = T.unpack . fromJust . getField . output $ test

    (out, stats) <- run R.Runner "./a.out" [] testInput
    status <- if out == testOutput then return OK else return ANS
    return TestResult
      { sourceTestId  = testId test
      , status        = putField . Just $ status
      , executionTime = putField . Just . round $ R.userTime stats
      , ramUsage      = putField . Just $ R.maxMemory stats
      }
