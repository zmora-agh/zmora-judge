{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings #-}

module Runnable where

import           Control.Monad.IO.Class
import           System.Exit                (ExitCode)
import           System.Process             (readProcessWithExitCode)
import           Configuration              (zmoraRunnerPath)
import           Data.Aeson.Types
import           Data.Aeson                 (decode)
import           Data.Maybe
import           Data.ByteString.Lazy.Char8 (pack)

type ProcessOut = (ExitCode, String, String)

class Runnable o where
  run :: FilePath -> [String] -> String -> o

instance (MonadIO m) => Runnable (m ProcessOut) where
  run path args input = liftIO $ readProcessWithExitCode path args input

data RunnerOutput = RunnerOutput {
  exitCode :: Int,
  maxMemory :: Int,
  systemTime :: Float,
  userTime :: Float,
  terminatedNormally :: Bool
} deriving Show


instance FromJSON RunnerOutput where
 parseJSON (Object v) =
    RunnerOutput <$> v .: "exit_code"
                 <*> v .: "max_memory"
                 <*> v .: "system_time"
                 <*> v .: "user_time"
                 <*> v .: "terminated_normally"
 parseJSON invalid = typeMismatch "RunnerOutput" invalid

instance (MonadIO m) => Runnable (m (String, RunnerOutput)) where
  run path args input = liftIO $ do
    (_, out, err) <- readProcessWithExitCode zmoraRunnerPath ([path] ++ args) input
    return $ (out, parseRunnerOutput err)

parseRunnerOutput :: String -> RunnerOutput
parseRunnerOutput input = fromJust $ decode $ pack input


