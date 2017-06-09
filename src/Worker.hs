{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module Worker where

import           Compiler
import           Control.Monad               (forM)
import           Control.Monad.Logger        (MonadLoggerIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString             as BS
import qualified Data.Text                   as T
import           Jail
import           RabbitMQ
import           System.Directory
import           System.IO.Temp
import           Data.ProtocolBuffers
import           Data.Maybe

import           Zmora.Queue

startWorker :: (MonadLoggerIO m, MonadBaseControl IO m) => Maybe String -> m ()
startWorker uri = startRabbitMQWorker uri processTask

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
  compile <- withCompiler (defaultPreset :: GCC) $ compile filenames "a.out"

  --TODO redo interface for running test
  forM tests $ \test -> withJail $ do
    let testInput  = T.unpack . fromJust . getField . input $ test
        testOutput = T.unpack . fromJust . getField . output $ test

    (_, out, _) <- run "./a.out" [] testInput
    status <- if out == testOutput then return OK else return ANS
    return TestResult
      { sourceTestId  = testId test
      , status        = putField . Just $ status
      , executionTime = putField . Just $ 0
      , ramUsage      = putField . Just $ 0
      }
