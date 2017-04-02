{-# LANGUAGE OverloadedStrings #-}

module Worker where

import ZeroMQ
import Json
import Models
import Compiler
import System.Directory

import System.IO.Temp

startWorker :: IO ()
startWorker = start0MQWorker $ parseTask processTask

processTask :: Task -> IO TaskResult
processTask task = withSystemTempDirectory "zmora-judge" $ \directory -> do
    setCurrentDirectory directory
    exampleProblemJudge $ source task


fromRight (Right a) = a



exampleProblemJudge :: Source -> IO TaskResult
exampleProblemJudge input = do
--     save input "source.c"

    result <- withCompiler (defaultPreset :: GCC) $ do
        blacklist "/usr/include/X11"
        compile "source.c" "a.out"

    return $ case result of
        CompilationOK -> TaskResult "Success"
        CompilationError error -> TaskResult error