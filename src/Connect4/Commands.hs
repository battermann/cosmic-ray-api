module Connect4.Commands
  ( Command (..),
    decide,
  )
where

import Connect4.Board
import Connect4.Events (Event (GameCreated, GameJoined, GameTied, GameWon, RedPlayed, YellowPlayed))
import Connect4.GameState
import Connect4.Types

data Command
  = CreateNewGame ClientId Color
  | JoinGame ClientId
  | Play ClientId Column

decide :: StreamId -> Command -> GameState -> Either Error [Event]
decide streamId cmd state = case (cmd, state) of
  (CreateNewGame p c, Uninitialized) -> Right [GameCreated streamId p c]
  (CreateNewGame _ _, _) -> Left StreamAlreadyExists
  (JoinGame p1, AwaitOpponent p2 _) ->
    if p1 /= p2
      then Right [GameJoined streamId p1]
      else Left JoinGameNotPossible
  (Play p col, RedToPlay r _ b) ->
    if p == r
      then case evalMove col b of
        Won -> Right [RedPlayed streamId col, GameWon streamId p]
        Tied -> Right [RedPlayed streamId col, GameTied streamId]
        InProgress -> Right [RedPlayed streamId col]
        Invalid -> Left IllegalMove
      else Left WrongPlayer
  (Play p col, YellowToPlay _ y b) ->
    if p == y
      then case evalMove col b of
        Won -> Right [YellowPlayed streamId col, GameWon streamId p]
        Tied -> Right [YellowPlayed streamId col, GameTied streamId]
        InProgress -> Right [YellowPlayed streamId col]
        Invalid -> Left IllegalMove
      else Left WrongPlayer
  (Play _ _, _) -> Left PlayNotPossible
  (JoinGame _, _) -> Left JoinGameNotPossible

data MoveResult = Won | Tied | InProgress | Invalid

evalMove :: Column -> Board -> MoveResult
evalMove col b =
  if isLegalMove col b
    then
      let x = play col b
       in if isWonGame x
            then Won
            else
              if isTiedGame x
                then Tied
                else InProgress
    else Invalid
