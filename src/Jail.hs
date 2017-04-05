{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Jail (withJail, setRamLimit, setCpuLimit, run, RLimit(..)) where

import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class
import System.Exit
import System.Process
import System.Directory

data RLimit = RUnlimited | RDefault | RLimit Int deriving Show

data NsJail = NsJail {
    jailPath :: FilePath,
    ramLimit :: RLimit,
    cpuLimit :: RLimit,
    chrootDir :: String
} deriving Show

class HasDefaultPreset a where
  defaultPreset :: IO a

instance HasDefaultPreset NsJail where
  defaultPreset = do
    cwd <- getCurrentDirectory
    return $ NsJail "/home/maxmati/src/foss/nsjail/nsjail" RDefault RDefault cwd

class Jail j m where
    setRamLimit :: RLimit -> StateT j m ()
    setCpuLimit :: RLimit -> StateT j m ()
    run :: FilePath -> [String] -> StateT j m (ExitCode, String, String)

instance MonadIO m => Jail NsJail m where
    setRamLimit ram =  modify $ \j -> j {ramLimit = ram}
    setCpuLimit cpu =  modify $ \j -> j {cpuLimit = cpu}

    run program args = do
        j <- get
        let nsArgs = [ "--chroot", chrootDir j
                     , "--rlimit_as", rlimitString $ ramLimit j
                     , "--rlimit_cpu", rlimitString $ cpuLimit j
                     , "--", program ] ++ args
        liftIO $ readProcessWithExitCode (jailPath j) nsArgs ""

rlimitString :: RLimit -> String
rlimitString rlimit = case rlimit of
    RUnlimited -> "max"
    RDefault -> "def"
    RLimit value -> show value

withJail :: StateT NsJail IO a -> IO a
withJail proc = do
    preset <- defaultPreset :: IO NsJail
    evalStateT proc preset

main :: IO ()
main = do
    res <- withJail $ do
        setRamLimit $ RLimit 10
        run "a.out" []

    print res
    return ()



