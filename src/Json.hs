{-# LANGUAGE OverloadedStrings #-}

module Json where

import Models
import Data.ByteString.Lazy
import Data.Aeson
import Data.Maybe

parseTask :: (Task -> TaskResult) -> ByteString -> ByteString
parseTask executor input = encode $ executor task
    where task = fromJust $ decode input


