{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Models where

import Data.Int (Int64)
import GHC.Generics
import Data.Aeson

type Source = String

type Program = FilePath

data CompilerParam = Param String | OutputFile | SourceCode deriving(Show)

data CompilerConfig = CompilerConfig {
    compiler :: FilePath,
    args :: [CompilerParam],
    stdin :: CompilerParam
} deriving(Show)

data Language = C | CPP deriving (Show, Generic, FromJSON)

data Task = Task {
    id :: Int,
    source :: Source,
    language :: Language
} deriving (Show, Generic, FromJSON)

newtype TaskResult = TaskResult {
    result :: String
} deriving (Show, Generic, ToJSON)

