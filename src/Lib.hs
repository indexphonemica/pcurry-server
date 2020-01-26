{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( initialize
    , startApp
    , shutdownApp
    , App
    , AppT (..)
    , app
    ) where

import           Control.Exception           (bracket)

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Int                    (Int64)
import qualified Data.Pool                   as Pool
import           Database.Persist.Postgresql (Entity, Filter, fromSqlKey, insert,
                                              selectList, entityVal)
import qualified Katip
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Safe                        (readMay)
import           Servant
import           System.Environment          (lookupEnv)


import           Config
import           Db                          (initDb, runDb)
import           Logger
import           Schema                      (User)

type API = "users" :> Get '[JSON] [User]
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64

appToServer :: Config -> Server API
appToServer cfg = hoistServer api (convertApp cfg) server

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runAppT appt) cfg

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
startApp :: IO ()
startApp = bracket acquireConfig shutdownApp runApp
  where
    runApp config = run (configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize cfg = do
    let logger = Config.setLogger (configEnv cfg)
    initDb $ configPool cfg
    pure . logger . app $ cfg

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
    port <- lookupSetting "PORT" 8081
    env  <- lookupSetting "ENV" Development
    logEnv <- defaultLogEnv
    pool <- makePool env logEnv
    pure Config
        { configPool = pool
        , configEnv = env
        , configLogEnv = logEnv
        , configPort = port
        }

shutdownApp :: Config -> IO ()
shutdownApp cfg = do
    _ <- Katip.closeScribes (configLogEnv cfg)
    Pool.destroyAllResources (configPool cfg)
    pure ()

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

app :: Config -> Application
app cfg = serve api (appToServer cfg)

api :: Proxy API
api = Proxy

server :: MonadIO m => ServerT API (AppT m)
server = users :<|> createUser

users :: MonadIO m => AppT m [User]
users = do
  entities <- runDb $ selectList ([] :: [Filter User]) []
  return $ map entityVal entities

createUser :: MonadIO m => User -> AppT m Int64
createUser usr = do
  k <- runDb $ insert usr
  return $ fromSqlKey k
