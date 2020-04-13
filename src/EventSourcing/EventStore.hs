{-# LANGUAGE DeriveGeneric #-}

module EventSourcing.EventStore
  ( EventStore (..),
    Version (..),
    handleCommand,
  )
where

import           Control.Monad.Except
import           Data.Aeson
import           GHC.Generics

newtype Version = Version Int
  deriving (Eq, Show, Generic)

instance ToJSON Version

data EventStore f streamId error event = EventStore
    { appendToStream :: streamId -> Version -> [event] -> ExceptT error f ()
    , readFromStream :: streamId -> ExceptT error f [(event, Version)]
    }

replay :: (event -> state -> state) -> state -> [(event, Version)] -> (state, Version)
replay apply game =
  foldl
    (\(state, _) (event, version) -> (apply event state, version))
    (game, Version (-1))

handleCommand ::
  Monad f =>
  EventStore f streamId error event ->
  (streamId -> cmd -> state -> Either error [event]) ->
  (event -> state -> state) ->
  state ->
  streamId ->
  cmd ->
  ExceptT error f ()
handleCommand eventStore decide apply initial streamId cmd = do
  events <- readFromStream eventStore streamId
  let (state, version) = replay apply initial events
  newEvents <- liftEither (decide streamId cmd state)
  appendToStream eventStore streamId version newEvents
