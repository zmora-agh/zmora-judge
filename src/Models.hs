{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Models where

import Data.Int (Int64)
import GHC.Generics
import Data.Aeson

newtype Task = Task {
    id :: Int
} deriving (Show, Generic, FromJSON)

newtype TaskResult = TaskResult {
    result :: String
} deriving (Show, Generic, ToJSON)

