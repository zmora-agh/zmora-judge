{-# LANGUAGE OverloadedStrings #-}

module Worker where

import ZeroMQ
import Json
import Models
import Compiler

import System.IO.Temp

startWorker :: IO ()
startWorker = start0MQWorker $ parseTask processTask

processTask :: Task -> IO TaskResult
processTask task = withSystemTempDirectory "zmora-judge" $ \directory -> do
    program <- compile task directory
    let test = fromRight program
    return $ TaskResult test

fromRight (Right a) = a