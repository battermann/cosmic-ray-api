{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Connect4.Events where

import Connect4.Board
import Connect4.GameState
import Connect4.Types
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Internal as BL (unpackChars)
import GHC.Generics

data Event
  = GameCreated StreamId ClientId Color
  | GameJoined StreamId ClientId
  | YellowPlayed StreamId Column
  | RedPlayed StreamId Column
  | GameWon StreamId ClientId
  | GameTied StreamId
  deriving (Eq, Generic)

instance ToJSON Event

instance FromJSON Event

instance Show Event where
  show = BL.unpackChars . encodePretty

apply :: Event -> GameState -> GameState
apply event game = case (event, game) of
  (GameCreated _ p c, Uninitialized) -> AwaitOpponent p c
  (GameJoined _ p1, AwaitOpponent p2 c) ->
    case c of
      -- yellow always plays first
      Yellow -> YellowToPlay p1 p2 empty
      Red -> YellowToPlay p2 p1 empty
  (YellowPlayed _ col, YellowToPlay y r b) ->
    RedToPlay y r (play col b)
  (RedPlayed _ col, RedToPlay y r b) ->
    YellowToPlay y r (play col b)
  (GameWon _ _, _) -> GameOver
  (GameTied _, _) -> GameOver
  _ -> game
