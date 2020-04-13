{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Requests where

import           Connect4.Types
import           Data.Aeson
import           GHC.Generics

data NewGame = NewGame
    { clientId :: ClientId
    , color    :: Color
    }
    deriving (Show, Generic)

instance FromJSON NewGame

newtype JoinGame = JoinGame {clientId :: ClientId}
  deriving (Show, Generic)

instance FromJSON JoinGame

data Play = Play
    { clientId :: ClientId
    , column   :: Column
    }
    deriving (Show, Generic)

instance FromJSON Play
