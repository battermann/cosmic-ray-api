{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Connect4.PostgresEventStore
  ( postgresEventStore,
    IOResult,
  )
where

import Connect4.Events (Event)
import Connect4.Types
import Contravariant.Extras.Contrazip
import Control.Monad
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators
import Data.Functor
import Data.Functor.Contravariant
import Data.List.Index as L (imap)
import Data.Vector as Vector
import EventSourcing.EventStore
import GHC.Generics
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Session as Session hiding (sql)
import Hasql.Statement (Statement (..))
import qualified Hasql.Transaction as Tx
import Hasql.Transaction.Sessions

type IOResult a = ExceptT Error IO a

postgresEventStore :: Connection.Connection -> EventStore IO StreamId Error Event
postgresEventStore conn =
  EventStore
    (\streamId version events -> ExceptT $ appendEvents conn streamId version events)
    (ExceptT . readStream conn)

streamIdEncoder :: E.Value StreamId
streamIdEncoder = contramap (\(StreamId streamId) -> streamId) E.uuid

versionEncoder :: E.Value Version
versionEncoder = contramap (\(Version version) -> fromIntegral version) E.int4

eventEncoder :: E.Value Event
eventEncoder = contramap toJSON E.json

---- READ ----

eventDecoder :: D.Value Event
eventDecoder =
  D.custom (\_ str -> maybeToRight "invalid event format" (decode (BL.fromStrict str)))

readStreamStatement :: Statement StreamId [(Event, Version)]
readStreamStatement = Statement sql encoder decoder True
  where
    sql = "SELECT data, version FROM events WHERE stream_id = $1 ORDER BY version"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.rowList ((,) <$> D.column (D.nonNullable eventDecoder) <*> (Version . fromIntegral <$> D.column (D.nonNullable D.int4)))

readStreamSession :: StreamId -> Session [(Event, Version)]
readStreamSession streamId = Session.statement streamId readStreamStatement

readStream :: Connection.Connection -> StreamId -> IO (Either Error [(Event, Version)])
readStream conn streamId = do
  dbResult <- run (readStreamSession streamId) conn
  return $ mapLeft (DbQueryError . show) dbResult

---- WRITE ----

createStreamStatement :: Statement StreamId ()
createStreamStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO streams (stream_id) VALUES ($1) ON CONFLICT (stream_id) DO NOTHING"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.noResult

getVersionStatement :: Statement StreamId Version
getVersionStatement = Statement sql encoder decoder True
  where
    sql = "SELECT max(version) FROM events WHERE stream_id = $1"
    encoder = E.param (E.nonNullable streamIdEncoder)
    decoder = D.singleRow (Version . maybe (-1) fromIntegral <$> D.column (D.nullable D.int4))

appendEventsStatement :: Statement (Vector (StreamId, Event, Version)) ()
appendEventsStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO events (stream_id, data, version) SELECT * FROM UNNEST ($1, $2, $3)"
    encoder =
      contramap Vector.unzip3 $
        contrazip3 (vector streamIdEncoder) (vector eventEncoder) (vector versionEncoder)
      where
        vector =
          E.param
            . E.nonNullable
            . E.array
            . E.dimension Vector.foldl'
            . E.element
            . E.nonNullable
    decoder = D.noResult

data VersionedEvent = VersionedEvent Version Event
  deriving (Eq, Show, Generic)

instance ToJSON VersionedEvent

appendEventsTransaction :: StreamId -> Version -> [Event] -> Tx.Transaction ()
appendEventsTransaction streamId expectedVersion events = do
  when (expectedVersion == Version (-1)) $ Tx.statement streamId createStreamStatement
  currentVersion <- Tx.statement streamId getVersionStatement
  when (currentVersion == expectedVersion) $ do
    Tx.statement (toVector expectedVersion) appendEventsStatement
    let notificationPayloads = BL.toStrict . encode <$> toVersioned expectedVersion
    (\p -> Tx.sql ("NOTIFY events, '" <> p <> "'")) `traverse` notificationPayloads $> ()
  where
    toVersioned :: Version -> [VersionedEvent]
    toVersioned (Version v) = L.imap (\i -> VersionedEvent (Version $ v + i + 1)) events
    toVector :: Version -> Vector (StreamId, Event, Version)
    toVector (Version v) = fromList $ L.imap (\i e -> (streamId, e, Version $ v + i + 1)) events

appendEventsSession :: StreamId -> Version -> [Event] -> Session ()
appendEventsSession streamId version events =
  transaction Serializable Write (appendEventsTransaction streamId version events)

appendEvents :: Connection.Connection -> StreamId -> Version -> [Event] -> IO (Either Error ())
appendEvents conn streamId version events = do
  dbResult <- run (appendEventsSession streamId version events) conn
  return $ mapLeft (DbQueryError . show) dbResult
