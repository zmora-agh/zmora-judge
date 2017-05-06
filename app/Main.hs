module Main where

import System.Environment (getArgs)
import Worker

main :: IO ()
main = do
  args <- getArgs
  case args of (brokerURI : _) -> startWorker (Just brokerURI)
               _               -> startWorker Nothing
