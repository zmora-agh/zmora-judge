{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Configuration
   ( gccPath,
     nsJailPath,
     zmoraRunnerPath,
     executionTimeoutMult,
     defaultRMQConnURI,
     validateConfiguration
   ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           System.Exit
import           System.IO.Error
import           System.Process         (readProcessWithExitCode)

gccPath :: String
gccPath = "/usr/bin/gcc"

nsJailPath :: String
nsJailPath = "/usr/bin/nsjail"

zmoraRunnerPath :: String
zmoraRunnerPath = "/usr/bin/zmora_runner"

-- Runner timeout termination factor for single test execution
-- Example: if test has CPU time limit set to 1s, forced termination will happen
-- after 1s*5=5s (when executionTimeoutMult is set to 5).
executionTimeoutMult :: Int
executionTimeoutMult = 5

defaultRMQConnURI :: T.Text
defaultRMQConnURI = "amqp://guest:guest@localhost:5672"

validateConfiguration :: (MonadLoggerIO m) => m ()
validateConfiguration = do
  validateGCC
  validateNsJail

validateExecutable :: MonadLoggerIO m => FilePath -> [String] -> m ()
validateExecutable path params = do
  result <- liftIO $ tryIOError $ readProcessWithExitCode path params ""
  verifyExecution (T.pack path) result

validateNsJail :: (MonadLoggerIO m ) => m ()
validateNsJail = validateExecutable nsJailPath ["-h"]

validateGCC :: (MonadLoggerIO m ) => m ()
validateGCC = validateExecutable gccPath ["--version"]

verifyExecution :: (MonadLoggerIO m ) => T.Text -> Either IOError (ExitCode, String, String) -> m ()
verifyExecution path (Right (_, stdout, stderr)) =
  $(logInfo) $ "Discovered " <> path <> ". Stdout:\n" <> T.pack stdout <>
    "\nStderr:\n" <> T.pack stderr
verifyExecution path (Left err) = do
  $(logError) $ "Failed to run " <> path <> ".\n Error:" <> (T.pack . show) err
  liftIO exitFailure
