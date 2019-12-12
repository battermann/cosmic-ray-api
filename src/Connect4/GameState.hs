module Connect4.GameState where

import Connect4.Board
import Connect4.Types

data GameState
  = Uninitialized
  | AwaitOpponent {player :: ClientId, color :: Color}
  | YellowToPlay {yellow :: ClientId, red :: ClientId, board :: Board}
  | RedToPlay {yellow :: ClientId, red :: ClientId, board :: Board}
  | GameOver
