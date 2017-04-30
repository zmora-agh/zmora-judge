{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Models where

import qualified Data.ByteString.Lazy as B
import           Data.Int             (Int64)
import           Data.MessagePack
import           GHC.Generics

type Source = String

type Program = FilePath

data CompilerParam = Param String | OutputFile | SourceCode deriving(Show)

data CompilerConfig = CompilerConfig {
    compiler :: FilePath,
    args     :: [CompilerParam],
    stdin    :: CompilerParam
} deriving(Show)

data Task = Task {
    taskId        :: Int,
    configuration :: String,
    files         :: [File],
    tests         :: [Test]
} deriving (Show, Generic)

instance MessagePack Task

data File = File {
  name    :: String,
  content :: B.ByteString
} deriving (Show, Generic)

instance MessagePack File


data Test = Test {
  input     :: String,
  output    :: String,
  timeLimit :: Int,
  ramLimit  :: Int
} deriving (Show, Generic)

instance MessagePack Test

data TaskResult = TaskResult {
  resultId       :: Int,
  compilationLog :: String,
  testResults    :: [TestResult]
} deriving (Show, Generic)

instance MessagePack TaskResult

data TestResult = TestResult {
  status        :: Status,
  executionTime :: Int,
  ramUsage      :: Int
} deriving (Show, Generic)

instance MessagePack TestResult

data Status = OK | RTE | MEM | TLE | ANS | CME deriving (Show, Generic)

instance MessagePack Status
