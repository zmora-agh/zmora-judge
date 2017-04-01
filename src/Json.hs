{-# LANGUAGE OverloadedStrings #-}

module Json where

import Models
import Data.ByteString.Lazy
import Data.Aeson
import Data.Maybe

parseTask :: (Task -> IO TaskResult) -> ByteString -> IO ByteString
parseTask executor input =  do
    result <- executor task
    return $ encode result
    where task = fromJust $ decode input


