{-# LANGUAGE OverloadedStrings #-}

module Worker where

import           Control.Monad (forM)
import           Compiler
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import           Data.MessagePack
import           Jail
import           RabbitMQ
import           System.Directory
import           System.IO.Temp
import           Zmora.Queue

startWorker :: IO ()
startWorker = startRabbitMQWorker processTask

processTask :: B.ByteString -> IO B.ByteString
processTask rawTask = withSystemTempDirectory "zmora-judge" $ \directory -> do
  task <- unpack rawTask
  setCurrentDirectory directory
  testsResults <- exampleProblemJudge (files task) (tests task)
  print testsResults
  let result = TaskResult (taskId task) "" testsResults
  return $ pack result

fromRight (Right a) = a

save :: File -> IO ()
save file = B.writeFile (T.unpack . name $ file) (content file)

saveAll :: [File] -> IO ()
saveAll = mapM_ save

exampleProblemJudge :: [File] -> [Test] -> IO [TestResult]
exampleProblemJudge files tests = do
  saveAll files

  compile <- withCompiler (defaultPreset :: GCC) $
    compile "source.c" "a.out"

  --TODO redo interface for running test
  forM tests $ \test -> withJail $ do
    (_, out, _) <- run "./a.out" [] $ input test
    status <- if out == output test
      then return OK
      else return ANS
    return $ TestResult status 0 0
