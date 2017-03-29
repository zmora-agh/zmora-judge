module Worker where

import ZeroMQ
import Json
import Models

startWorker :: IO ()
startWorker = start0MQWorker $ parseTask (\x -> TaskResult "Szatan czyste zło")

