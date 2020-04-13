module Connect4.InMemoryEventStore
  ( newInMemoryEventStore,
    IOResult,
  )
where

import           Connect4.Events            (Event)
import           Connect4.Types
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Functor
import           Data.IORef
import           Data.List.Index            (imap)
import           Data.Map.Strict            as Map (Map, empty, insert, lookup)
import           Data.Maybe
import           EventSourcing.EventStore

type IOResult a = ExceptT Error IO a

newInMemoryEventStore :: IO (EventStore IO StreamId Error Event)
newInMemoryEventStore = inMemoryEventStore <$> newIORef Map.empty

readStream :: IORef (Map StreamId [Event]) -> StreamId -> IOResult [(Event, Version)]
readStream ref streamId = do
  streams <- liftIO $ readIORef ref
  let events = fromMaybe [] $ Map.lookup streamId streams
  return $ imap (\i e -> (e, Version i)) events

update :: StreamId -> Version -> [Event] -> Map StreamId [Event] -> (Map StreamId [Event], Either Error ())
update streamId (Version version) events streams =
  case Map.lookup streamId streams of
    Nothing ->
      if version == -1
        then (Map.insert streamId events streams, Right ())
        else (streams, Left StreamNotFound)
    Just events' ->
      if length events' == version + 1
        then (Map.insert streamId (events' ++ events) streams, Right ())
        else (streams, Left ConcurrentWriteError)

append :: IORef (Map StreamId [Event]) -> StreamId -> Version -> [Event] -> IOResult ()
append ref streamId version events = do
  result <- ExceptT $ atomicModifyIORef ref (update streamId version events)
  liftIO $ print streamId
  liftIO $ lLog events
  return result

inMemoryEventStore :: IORef (Map StreamId [Event]) -> EventStore IO StreamId Error Event
inMemoryEventStore ref = EventStore (append ref) (readStream ref)

lLog :: Show a => [a] -> IO ()
lLog xs = traverse print xs $> ()
