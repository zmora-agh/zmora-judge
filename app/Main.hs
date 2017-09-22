module Main where

import           Configuration               (validateConfiguration,
                                              defaultRMQConnURI)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Text                   as T
import           System.Environment          (getArgs)
import           Worker

main :: IO ()
main = runStderrLoggingT $ do
  validateConfiguration
  args <- liftIO getArgs
  case args of
    (brokerURI:_) -> startWorker . T.pack $ brokerURI
    _             -> startWorker defaultRMQConnURI
