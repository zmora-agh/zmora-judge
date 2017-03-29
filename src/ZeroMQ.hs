module ZeroMQ
    ( start0MQWorker
    ) where


import Control.Concurrent
import Control.Monad
import Data.Monoid
import System.ZMQ4.Monadic
import qualified Data.ByteString.Lazy as BL


start0MQWorker :: (BL.ByteString -> BL.ByteString) -> IO ()
start0MQWorker executor = runZMQ $ do
    receiver <- socket Pull
    connect receiver "tcp://localhost:5557"


    sender <- socket Push
    connect sender "tcp://localhost:5558"

    forever $ do
            string <- receive receiver
            send sender [] $ BL.toStrict $ executor  $ BL.fromStrict string