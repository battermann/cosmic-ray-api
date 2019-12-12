{-# LANGUAGE DeriveGeneric #-}

module Connect4.Types where

import Data.Aeson
import Data.UUID
import GHC.Generics

data Color
  = Yellow
  | Red
  deriving (Eq, Show, Generic)

instance FromJSON Color

instance ToJSON Color

newtype ClientId = ClientId UUID deriving (Eq, Show, Generic)

instance FromJSON ClientId

instance ToJSON ClientId

newtype StreamId = StreamId UUID deriving (Eq, Ord, Show, Generic)

instance FromJSON StreamId

instance ToJSON StreamId

newtype Column = Column Int deriving (Eq, Ord, Show, Generic)

instance FromJSON Column

instance ToJSON Column

data Error
  = StreamNotFound
  | ConcurrentWriteError
  | StreamAlreadyExists
  | JoinGameNotPossible
  | WrongPlayer
  | IllegalMove
  | PlayNotPossible
  | DbQueryError String
  deriving (Eq, Show)
