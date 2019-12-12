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

ok :: a -> Either b a
ok = Right

err :: b -> Either b a
err = Left

decide :: StreamId -> Command -> GameState -> Either Error [Event]
decide streamId cmd state = case (cmd, state) of
  (CreateNewGame player color, Uninitialized) -> ok [GameCreated streamId player color]
  (CreateNewGame _ _, _) -> err StreamAlreadyExists
  (JoinGame player1, AwaitOpponent player2 _) ->
    if player1 /= player2
      then ok [GameJoined streamId player1]
      else err JoinGameNotPossible
  (Play player column, RedToPlay red _ board) ->
    if player == red
      then case evalMove column board of
        Won -> ok [RedPlayed streamId column, GameWon streamId player]
        Tied -> ok [RedPlayed streamId column, GameTied streamId]
        InProgress -> ok [RedPlayed streamId column]
        Invalid -> err IllegalMove
      else err WrongPlayer
  (Play player column, YellowToPlay _ yellow board) ->
    if player == yellow
      then case evalMove column board of
        Won -> ok [YellowPlayed streamId column, GameWon streamId player]
        Tied -> ok [YellowPlayed streamId column, GameTied streamId]
        InProgress -> ok [YellowPlayed streamId column]
        Invalid -> err IllegalMove
      else err WrongPlayer
  (Play _ _, _) -> err PlayNotPossible
  (JoinGame _, _) -> err JoinGameNotPossible

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
