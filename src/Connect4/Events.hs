{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Connect4.Events where

import           Connect4.Board
import           Connect4.GameState
import           Connect4.Types
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Internal as BL (unpackChars)
import           Data.Time.Clock               (UTCTime)
import           GHC.Generics

data Event = GameCreated StreamId UTCTime ClientId Color
    | GameJoined StreamId UTCTime ClientId
    | YellowPlayed StreamId UTCTime Column
    | RedPlayed StreamId UTCTime Column
    | GameWon StreamId UTCTime ClientId
    | GameTied StreamId UTCTime
    deriving (Eq, Generic)

instance ToJSON Event

instance FromJSON Event

instance Show Event where
  show = BL.unpackChars . encodePretty

apply :: Event -> GameState -> GameState
apply event game = case (event, game) of
  (GameCreated _ _ player color, Uninitialized) -> AwaitOpponent player color
  (GameJoined _ _ player1, AwaitOpponent player2 color) ->
    case color of
      Yellow -> YellowToPlay player1 player2 empty
      Red    -> YellowToPlay player2 player1 empty
  (YellowPlayed _ _ column, YellowToPlay yellow red board) ->
    RedToPlay yellow red (play column board)
  (RedPlayed _ _ column, RedToPlay yellow red board) ->
    YellowToPlay yellow red (play column board)
  (GameWon _ _ _, _) -> GameOver
  (GameTied _ _, _) -> GameOver
  _ -> game
