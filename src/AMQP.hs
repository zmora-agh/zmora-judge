{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}

module AMQP where

import           Control.Monad.Base
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy        as BS
import qualified Data.Text                   as T
import           Network.AMQP                hiding (consumeMsgs)
import           Network.AMQP.Lifted

connect :: ConnectionOpts -> IO Channel
connect connectionOpts = openConnection'' connectionOpts >>= openChannel

withConnection :: ConnectionOpts -> (Connection -> IO a) -> IO a
withConnection opts f = do
  connection <- openConnection'' opts
  res <- f connection
  closeConnection connection
  return res

data PublisherSpec m = PublisherSpec
  { pubExchangeOpts :: Maybe ExchangeOpts
  , pubKey          :: T.Text
  , pubSerializer   :: m -> BS.ByteString
  , pubAwaitNanos   :: Maybe Int
  }

data Publisher m = Publisher
  { pubSpec :: PublisherSpec m
  , pubChan :: Channel
  }

data SubscriberSpec m a = SubscriberSpec
  { subOpts         :: QueueOpts
  , subDeserializer :: BS.ByteString -> m a
  }

data Subscriber m a = Subscriber
  { subSpec :: SubscriberSpec m a
  , subChan :: Channel
  }

newPublisher :: PublisherSpec m -> Channel -> IO (Publisher m)
newPublisher spec channel = do
  mapM_ (declareExchange channel) (pubExchangeOpts spec)
  return $ Publisher spec channel

connectPublisher :: ConnectionOpts -> PublisherSpec m -> IO (Publisher m)
connectPublisher opts spec = connect opts >>= newPublisher spec

newSubscriber :: SubscriberSpec m a -> Channel -> IO (Subscriber m a)
newSubscriber spec channel = do
  _ <- declareQueue channel (subOpts spec)
  return $ Subscriber spec channel

connectSubscriber :: ConnectionOpts -> SubscriberSpec m a -> IO (Subscriber m a)
connectSubscriber opts spec = connect opts >>= newSubscriber spec

publish :: Publisher m -> m -> IO (Maybe ConfirmationResult)
publish (Publisher (PublisherSpec opts key serializer awaitNanos) channel) msg = do
  _ <-
    publishMsg
      channel
      (maybe "" exchangeName opts)
      key
      newMsg {msgBody = serializer msg}
  mapM (waitForConfirmsUntil channel) awaitNanos

withPublisher
  :: MonadBaseControl IO m
  => Connection -> PublisherSpec a -> (Publisher a -> m b) -> m b
withPublisher connection spec f = do
  channel <- liftBase $ openChannel connection
  publisher <- liftBase $ newPublisher spec channel
  res <- f publisher
  liftBase $ closeChannel channel
  return res

subscribe
  :: (MonadBaseControl IO m)
  => Subscriber b t -> ((b t, Envelope) -> m ()) -> m ConsumerTag
subscribe (Subscriber (SubscriberSpec opts deserializer) channel) f =
  consumeMsgs
    channel
    (queueName opts)
    Ack
    (\(msg, env) -> f (deserializer $ msgBody msg, env))

--
-- Well-known exchange/queue declarations
--
taskQueueName :: T.Text
taskQueueName = "tasks"

taskQueueOpts :: QueueOpts
taskQueueOpts = newQueue {queueName = taskQueueName}

taskErrorQueueName :: T.Text
taskErrorQueueName = "tasksErrors"

taskErrorQueueOpts :: QueueOpts
taskErrorQueueOpts = newQueue {queueName = taskErrorQueueName}

taskResultQueueName :: T.Text
taskResultQueueName = "tasksResults"

taskResultQueueOpts :: QueueOpts
taskResultQueueOpts = newQueue {queueName = taskResultQueueName}

declareStandardQueues :: Channel -> IO ()
declareStandardQueues channel = mapM_ (declareQueue channel) stdQueues
  where
    stdQueues = [taskQueueOpts, taskErrorQueueOpts, taskResultQueueOpts]
