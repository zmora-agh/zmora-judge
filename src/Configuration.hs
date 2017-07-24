{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Configuration
   ( gccPath,
     nsJailPath,
     rabbitMQConnectionOpts,
     validateConfiguration
   ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Network.AMQP
import           System.Exit
import           System.IO.Error
import           System.Process         (readProcessWithExitCode)

gccPath :: [Char]
gccPath = "gcc"
nsJailPath :: [Char]
nsJailPath = "nsjail"

rabbitMQConnectionOpts :: ConnectionOpts
rabbitMQConnectionOpts = defaultConnectionOpts

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

verifyExecution :: (MonadLoggerIO m ) => T.Text -> (Either IOError (ExitCode, String, String)) -> m ()
verifyExecution path (Right (_, stdout, stderr)) =
  $(logInfo) $ "Discovered " <> path <> ". Stdout:\n" <> T.pack stdout <>
    "\nStderr:\n" <> T.pack stderr
verifyExecution path (Left err) = do
  $(logError) $ "Failed to run " <> path <> ".\n Error:" <> (T.pack . show) err
  liftIO $ exitFailure
