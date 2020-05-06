{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api
  ( startApp,
  )
where

import           Connect4.Commands             as Cmd (Command (..), decide)
import           Connect4.Events
import           Connect4.GameState            (GameState (Uninitialized))
import           Connect4.PostgresEventStore
import           Connect4.Types
import           Control.Monad.Except
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Time.Clock               (getCurrentTime)
import           Data.UUID
import qualified Data.UUID.V4                  as UUID
import           EventSourcing.EventStore
import qualified Hasql.Connection              as Connection
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Requests
import qualified Responses
import           Servant

type API =
  "games" :> ReqBody '[JSON] NewGame :> PostCreated '[JSON] Responses.GameCreated
    :<|> "games" :> Capture "gameId" UUID :> "join" :> ReqBody '[JSON] JoinGame :> Post '[JSON] NoContent
    :<|> "games" :> Capture "gameId" UUID :> "play" :> ReqBody '[JSON] Play :> Post '[JSON] NoContent

type CommandHandler = StreamId -> Command -> IOResult ()

startApp :: Int -> String -> IO ()
startApp port connectionString = do
  connectionOrError <- Connection.acquire $ C.pack connectionString
  case connectionOrError of
    Left err -> print err
    Right connection ->
      let eventStore = postgresEventStore connection
          handler = handleCommand eventStore decide apply Uninitialized
       in run port (app handler)

app :: CommandHandler -> Application
app handler = corsMiddleware (serve api (server handler))

corsMiddleware :: Middleware
corsMiddleware = cors $ const (Just policy)

policy :: CorsResourcePolicy
policy =
  CorsResourcePolicy
    { corsOrigins = Nothing,
      corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"],
      corsRequestHeaders = simpleHeaders,
      corsExposedHeaders = Nothing,
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }

api :: Proxy API
api = Proxy

server :: CommandHandler -> Server API
server handler =
  newGame
    :<|> joinGame
    :<|> play
  where
    newGame :: NewGame -> Handler Responses.GameCreated
    newGame req = do
      streamId <- liftIO $ StreamId <$> UUID.nextRandom
      time <- liftIO getCurrentTime
      let cmd = Cmd.CreateNewGame (clientId (req :: NewGame)) time (color (req :: NewGame))
      Handler $ withExceptT toServerError $ handler streamId cmd
      return $ Responses.GameCreated streamId
    joinGame :: UUID -> JoinGame -> Handler NoContent
    joinGame uuid req = do
      time <- liftIO getCurrentTime
      let cmd = Cmd.JoinGame (clientId (req :: JoinGame)) time
      Handler $ withExceptT toServerError $ handler (StreamId uuid) cmd
      return NoContent
    play :: UUID -> Play -> Handler NoContent
    play uuid req = do
      time <- liftIO getCurrentTime
      let cmd = Cmd.Play (clientId (req :: Play)) time (column (req :: Play))
      Handler $ withExceptT toServerError $ handler (StreamId uuid) cmd
      return NoContent

toServerError :: Error -> ServerError
toServerError err = err500 {errBody = BL.packChars $ show err}
