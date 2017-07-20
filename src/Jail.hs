{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Jail (withJail, setRamLimit, setCpuLimit, run, RLimit(..)) where

import           Configuration
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Lazy
import           System.Directory
import qualified Runnable as R

data RLimit = RUnlimited | RDefault | RLimit Int deriving Show

data NsJail = NsJail {
    jailPath  :: FilePath,
    ramLimit  :: RLimit,
    cpuLimit  :: RLimit,
    chrootDir :: String
} deriving Show

class HasDefaultPreset a where
  defaultPreset :: IO a

instance HasDefaultPreset NsJail where
  defaultPreset = do
    cwd <- getCurrentDirectory
    return $ NsJail nsJailPath RDefault RDefault cwd

class Jail j m where
    setRamLimit :: RLimit -> StateT j m ()
    setCpuLimit :: RLimit -> StateT j m ()
    run :: R.Runnable r a => r -> FilePath -> [String] -> String -> StateT j m a

instance MonadIO m => Jail NsJail m where
    setRamLimit ram =  modify $ \j -> j {ramLimit = ram}
    setCpuLimit cpu =  modify $ \j -> j {cpuLimit = cpu}

    run r program args input = do
        j <- get
        let nsArgs = [ "--chroot", chrootDir j
                     , "--rlimit_as", rlimitString $ ramLimit j
                     , "--rlimit_cpu", rlimitString $ cpuLimit j
                     , "-R", "/usr/bin", "-l", "/dev/null"
                     , "--", R.path r program args ] ++ R.args r program args
        R.run r (jailPath j) nsArgs input

rlimitString :: RLimit -> String
rlimitString rlimit = case rlimit of
    RUnlimited   -> "max"
    RDefault     -> "def"
    RLimit value -> show value

withJail :: StateT NsJail IO a -> IO a
withJail proc = do
    preset <- defaultPreset :: IO NsJail
    evalStateT proc preset
