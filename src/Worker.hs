{-# LANGUAGE OverloadedStrings #-}

module Worker where

import ZeroMQ
import Json
import Models
import Compiler
import Jail
import System.Directory
import System.Exit
import System.IO
import System.IO.Temp

startWorker :: IO ()
startWorker = start0MQWorker $ parseTask processTask

processTask :: Task -> IO TaskResult
processTask task = withSystemTempDirectory "zmora-judge" $ \directory -> do
    setCurrentDirectory directory
    exampleProblemJudge $ source task


fromRight (Right a) = a

save :: FilePath -> Source -> IO ()
save = writeFile


exampleProblemJudge :: Source -> IO TaskResult
exampleProblemJudge input = do
    save "source.c" input

    compile <- withCompiler (defaultPreset :: GCC) $ do
        blacklist "/usr/include/X11"
        compile "source.c" "a.out"

    print compile

    result <- withJail $ do
        setRamLimit $ RLimit 10
        setCpuLimit $ RLimit 1
        run "./a.out" []

    return $ TaskResult $ show result