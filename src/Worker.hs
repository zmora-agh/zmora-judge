{-# LANGUAGE OverloadedStrings #-}

module Worker where

import           Compiler
import qualified Data.ByteString.Lazy as B
import           Data.MessagePack
import           Jail
import           Models
import           RabbitMQ
import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Temp


startWorker :: IO ()
startWorker = startRabbitMQWorker processTask

processTask :: B.ByteString -> IO B.ByteString
processTask rawTask = withSystemTempDirectory "zmora-judge" $ \directory -> do
  task <- unpack rawTask
  setCurrentDirectory directory
  testsResults <- exampleProblemJudge (files task) (tests task)
  let result = TaskResult 15 "" testsResults
  return $ pack result


fromRight (Right a) = a

save :: File -> IO ()
save file = B.writeFile (name file) (content file)

saveAll :: [File] -> IO ()
saveAll = mapM_ save

foreach :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
foreach = flip mapM

exampleProblemJudge :: [File] -> [Test] -> IO [TestResult]
exampleProblemJudge files tests = do
  saveAll $ files

  compile <- withCompiler (defaultPreset :: GCC) $ do
    compile "source.c" "a.out"

  --TODO redo interface for running test
  foreach tests $ \test -> withJail $ do
    (_, out, _) <- run "./a.out" [] $ input test
    status <- if out == output test
      then return OK
      else return ANS
    return $ TestResult status 0 0
