{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

module Compiler where

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import System.Exit
import System.Process
import Data.List (delete)

data CompilationStatus = CompilationOK | CompilationError String deriving Show

type CompileT c m = StateT c m CompilationStatus

data GCC = GCC { gccPath :: String
               , optimisation :: String
               , includes :: [String]
               } deriving Show

class HasDefaultPreset a where
  defaultPreset :: a

instance HasDefaultPreset GCC where
  defaultPreset = GCC "/usr/bin/gcc" "-O2" ["/usr/include", "/usr/include/X11"]

class Compiler c m where
  compile :: FilePath -> FilePath -> CompileT c m
  blacklist :: String -> StateT c m ()

instance MonadIO m => Compiler GCC m where
  compile src out = do
    config <- get
    let args = [src, "-o", out, optimisation config]
    result <- liftIO $ readProcessWithExitCode (gccPath config) args ""
    return $ case result of
      (ExitSuccess, _, _) -> CompilationOK
      (_, _, error) -> CompilationError error

  blacklist x = modify $ \(GCC p opt incl) -> GCC p opt (delete x incl)

withCompiler :: Functor f => a -> StateT a f b -> f b
withCompiler compiler procedure = fst <$> runStateT procedure compiler

main :: IO ()
main = do
--  save input "source.c"
  resultGCC <- withCompiler (defaultPreset :: GCC) $ do
    blacklist "/usr/include/X11"
    compile "source.c" "a.out"

  print resultGCC