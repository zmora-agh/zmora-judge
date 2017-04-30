{-# LANGUAGE OverloadedStrings #-}

module Worker where

import           Compiler
import qualified Data.ByteString.Lazy as B
import           Data.MessagePack
import           Jail
import           Models
import           RabbitMQ
import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Temp
import Numeric (showHex)


startWorker :: IO ()
startWorker = startRabbitMQWorker processTask

prettyPrint :: B.ByteString -> String
prettyPrint = concat . map (flip showHex "") . B.unpack

processTask :: B.ByteString -> IO B.ByteString
processTask rawTask = withSystemTempDirectory "zmora-judge" $ \directory -> do
    task <- unpack rawTask
    setCurrentDirectory directory
    result <- exampleProblemJudge $ task
    return $ pack result


fromRight (Right a) = a

save :: FilePath -> Source -> IO ()
save = writeFile


exampleProblemJudge :: Task -> IO TaskResult
exampleProblemJudge task = do
--    save "source.c" task

    compile <- withCompiler (defaultPreset :: GCC) $ do
        blacklist "/usr/include/X11"
        compile "source.c" "a.out"

    print compile

    result <- withJail $ do
        setRamLimit $ RLimit 10
        setCpuLimit $ RLimit 1
        run "./a.out" []

    return $ TaskResult { resultId = 15, compilationLog = "", testResults = [] }
