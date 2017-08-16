{-# LANGUAGE FlexibleContexts #-}

module QueueModel (
    module QueueModel.Judge.Task
  , module QueueModel.Judge.Task.File
  , module QueueModel.Judge.Task.Test
  , module QueueModel.Judge.TaskResult
  , module QueueModel.Judge.TaskResult.Status
  , module QueueModel.Judge.TaskResult.TestResult
  , DeserializationException
  , task
  , file
  , taskResult
  , testResult
  , messageGetIO
  ) where

import           Control.Exception.Lifted               (Exception, throwIO)
import           Control.Monad.Base                     (MonadBase)
import           Data.Int                               (Int64)
import           Data.Maybe                             ()
import           Data.Sequence                          (fromList)
import qualified Data.ByteString.Lazy                   as B
import           Text.ProtocolBuffers

import           QueueModel.Judge.Task
import           QueueModel.Judge.Task.File
import           QueueModel.Judge.Task.Test
import           QueueModel.Judge.TaskResult
import           QueueModel.Judge.TaskResult.Status
import           QueueModel.Judge.TaskResult.TestResult

task :: Int64 -> [File] -> [Test] -> Task
task tId tFiles tTests = Task
  (Just tId)
  (fromList tFiles)
  (fromList tTests)

file :: Int64 -> String -> B.ByteString -> File
file fileId filename fContent =
  File
    (Just fileId)
    (Just . fromString $ filename)
    (Just fContent)

taskResult :: Int64 -> [TestResult] -> TaskResult
taskResult tId trResults = TaskResult
  (Just tId)
  (fromList trResults)

testResult :: Int64 -> Status -> Int64 -> Int64 -> Int64 -> TestResult
testResult tId tStatus uTime sTime mem =
  TestResult
    (Just tId)
    (Just tStatus)
    (Just uTime)
    (Just sTime)
    (Just mem)

data DeserializationException = DeserializationException B.ByteString String
instance Show DeserializationException where
  show (DeserializationException _ cause) =
    "deserialization failed: " ++ cause
instance Exception DeserializationException

messageGetIO :: (Wire a, ReflectDescriptor a, MonadBase IO m) =>
  B.ByteString -> m a
messageGetIO rawTask = either
  (throwIO . DeserializationException rawTask)
  (return . fst)
  (messageGet rawTask)

