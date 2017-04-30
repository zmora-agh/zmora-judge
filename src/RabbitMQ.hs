{-# LANGUAGE OverloadedStrings #-}
module RabbitMQ ( startRabbitMQWorker ) where

import           Configuration
import qualified Data.ByteString.Lazy as B
import           Network.AMQP

tasksQueueName = "tasks"
tasksResultsQueueName = "tasksResults"

startRabbitMQWorker :: (B.ByteString -> IO B.ByteString) -> IO ()
startRabbitMQWorker executor = do
  connection <- openConnection'' rabbitMQConnectionOpts

  receiveChannell <- openChannel connection
  qos receiveChannell 0 1 False
  declareQueue receiveChannell newQueue {queueName = tasksQueueName}

  sendingChannell <- openChannel connection
  declareQueue sendingChannell newQueue {queueName = tasksResultsQueueName}

  consumeMsgs receiveChannell tasksQueueName Ack $
    processMsg sendingChannell executor

  putStrLn "Press any key to exit."
  getLine
  closeConnection connection

processMsg :: Channel -> (B.ByteString -> IO B.ByteString) -> (Message, Envelope) -> IO ()
processMsg channel executor (msg, env) = do
  let body = msgBody msg

  result <- executor body
  --TODO redo it using new thread
  publishMsg channel "" tasksResultsQueueName newMsg { msgBody = result }
  ackEnv env
