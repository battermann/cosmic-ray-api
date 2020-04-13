{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Responses where

import           Connect4.Types
import           Data.Aeson
import           GHC.Generics

data GameCreated = GameCreated
    { gameId :: StreamId
    }
    deriving (Show, Generic)

instance ToJSON GameCreated
