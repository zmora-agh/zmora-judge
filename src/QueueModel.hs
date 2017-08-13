module QueueModel (
    module QueueModel.Judge.Task
  , module QueueModel.Judge.Task.File
  , module QueueModel.Judge.Task.Test
  , module QueueModel.Judge.TaskResult
  , module QueueModel.Judge.TaskResult.Status
  , module QueueModel.Judge.TaskResult.TestResult
  , task
  , file
  , taskResult
  , testResult
  ) where

import           Data.Int   (Int64)
import           Data.Maybe ()
import           Data.Sequence (fromList)
import qualified Data.ByteString.Lazy        as B
import           Text.ProtocolBuffers (fromString)

import           QueueModel.Judge.Task
import           QueueModel.Judge.Task.File
import           QueueModel.Judge.Task.Test
import           QueueModel.Judge.TaskResult
import           QueueModel.Judge.TaskResult.Status
import           QueueModel.Judge.TaskResult.TestResult

task :: Int64 -> [File] -> [Test] -> Task
task tId tFiles tTests = Task
  (Just tId)
  Nothing
  (fromList tFiles)
  (fromList tTests)

file :: String -> B.ByteString -> File
file fName fContent = File (Just . fromString $ fName) (Just fContent)

taskResult :: Int64 -> [TestResult] -> TaskResult
taskResult tId trResults = TaskResult
  (Just tId)
  Nothing
  (fromList trResults)

testResult :: Int64 -> Status -> Int64 -> Int64 -> TestResult
testResult tId tStatus time mem = TestResult
  (Just tId)
  (Just tStatus)
  (Just time)
  (Just mem)

