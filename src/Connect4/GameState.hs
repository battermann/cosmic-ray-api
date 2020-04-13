module Connect4.GameState where

import           Connect4.Board
import           Connect4.Types

data GameState = Uninitialized
    | AwaitOpponent ClientId Color
    | YellowToPlay ClientId ClientId Board
    | RedToPlay ClientId ClientId Board
    | GameOver
