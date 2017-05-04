{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler where

import qualified Configuration                  as C
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Lazy
import           Data.List                      (delete)
import           System.Exit
import           System.FilePath.Posix          (takeExtension)
import           System.Process

data CompilationStatus = CompilationOK | CompilationError String deriving Show

type CompileT c m = StateT c m CompilationStatus

data GCC = GCC { gccPath      :: String
               , optimisation :: String
               , includes     :: [String]
               } deriving Show

class HasDefaultPreset a where
  defaultPreset :: a

instance HasDefaultPreset GCC where
  defaultPreset = GCC C.gccPath "-O2" ["/usr/include", "/usr/include/X11"]

class Compiler c m where
  compile :: [FilePath] -> FilePath -> CompileT c m
  blacklist :: String -> StateT c m ()

instance MonadIO m => Compiler GCC m where
  compile src out = do
    config <- get
    let sources = filter (\filename -> takeExtension filename == ".c") src
    let args = sources ++ ["--static", "-o", out, optimisation config]
    result <- liftIO $ readProcessWithExitCode (gccPath config) args ""
    return $ case result of
      (ExitSuccess, _, _) -> CompilationOK
      (_, _, error)       -> CompilationError error

  blacklist x = modify $ \(GCC p opt incl) -> GCC p opt (delete x incl)

withCompiler :: Functor f => a -> StateT a f b -> f b
withCompiler compiler procedure = fst <$> runStateT procedure compiler
