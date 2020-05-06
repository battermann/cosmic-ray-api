module Connect4.Commands
  ( Command (..),
    decide,
  )
where

import           Connect4.Board
import           Connect4.Events    (Event (GameCreated, GameJoined, GameTied, GameWon, RedPlayed, YellowPlayed))
import           Connect4.GameState
import           Connect4.Types
import           Data.Time.Clock    (UTCTime)

data Command = CreateNewGame ClientId UTCTime Color
    | JoinGame ClientId UTCTime
    | Play ClientId UTCTime Column

decide :: StreamId -> Command -> GameState -> Either Error [Event]
decide streamId cmd state = case (cmd, state) of
  (CreateNewGame player time color, Uninitialized) -> ok [GameCreated streamId time player color]
  (CreateNewGame _ _ _, _) -> err StreamAlreadyExists
  (JoinGame player1 time, AwaitOpponent player2 _) ->
    if player1 /= player2
      then ok [GameJoined streamId time player1]
      else err JoinGameNotPossible
  (Play player time column, RedToPlay red _ board) ->
    if player == red
      then case evalMove column board of
        Won        -> ok [RedPlayed streamId time column, GameWon streamId time player]
        Tied       -> ok [RedPlayed streamId time column, GameTied streamId time]
        InProgress -> ok [RedPlayed streamId time column]
        Invalid    -> err IllegalMove
      else err WrongPlayer
  (Play player time column, YellowToPlay _ yellow board) ->
    if player == yellow
      then case evalMove column board of
        Won -> ok [YellowPlayed streamId time column, GameWon streamId time player]
        Tied -> ok [YellowPlayed streamId time column, GameTied  streamId time ]
        InProgress -> ok [YellowPlayed streamId time column]
        Invalid -> err IllegalMove
      else err WrongPlayer
  (Play _ _ _, _) -> err PlayNotPossible
  (JoinGame _ _, _) -> err JoinGameNotPossible

data MoveResult = Won
    | Tied
    | InProgress
    | Invalid

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

ok :: a -> Either b a
ok = Right

err :: b -> Either b a
err = Left
