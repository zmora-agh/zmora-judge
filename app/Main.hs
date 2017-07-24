module Main where

import           Configuration               (validateConfiguration)
import           Control.Monad.IO.Class
import           Control.Monad.Logger.Syslog (runSyslogLoggingT)
import           System.Environment          (getArgs)
import           Worker

main :: IO ()
main = do
  runSyslogLoggingT $ do
    validateConfiguration
    args <- liftIO $ getArgs
    case args of
      (brokerURI:_) -> startWorker (Just brokerURI)
      _             -> startWorker Nothing
