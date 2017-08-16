{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings #-}

module Runnable where

import           Control.Monad.IO.Class
import           Control.Exception          (throwIO)
import           System.Process             (readProcessWithExitCode)
import           Configuration              (zmoraRunnerPath)
import           Data.Aeson.Types
import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy.Char8 (pack)
import           GHC.Int                    (Int64)
import           System.Exit

type ProcessOut = (ExitCode, String, String)

class Runnable r o | r -> o where
  path :: r -> FilePath -> [String] -> FilePath
  args :: r -> FilePath -> [String] -> [String]
  run :: (MonadIO m) => r -> FilePath -> [String] -> String -> m o

instance Runnable Program ProcessOut where
  path _ p _ = p
  args _ _ a = a
  run _ p a input = liftIO $ readProcessWithExitCode p a input

data Program = Program


data Runner = Runner

data RunnerOutput = RunnerOutput {
  exitCode :: Int64,
  maxMemory :: Int64,
  systemTime :: Int64,
  userTime :: Int64,
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

instance Runnable Runner (String, RunnerOutput) where
  path _ _ _ = zmoraRunnerPath
  args _ p a = p : a
  run _ p a input = liftIO $ do
    (code, out, err) <- readProcessWithExitCode p a input
    case code of
      ExitSuccess -> return (out, parseRunnerOutput err)
      failure -> throwIO failure

parseRunnerOutput :: String -> RunnerOutput
parseRunnerOutput input = case decode $ pack input of
  Just res -> res
  Nothing -> error $ "Unable to parse runner output \n" ++ input


