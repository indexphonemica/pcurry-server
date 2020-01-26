{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where

import           Config                      (Config (..), Environment (..),
                                              makePool)
import           Control.Exception           (bracket, finally, throwIO)
import           Control.Monad.Except        (runExceptT)
import           Control.Monad.Reader        (runReaderT)
import           Database.Persist.Postgresql (Entity (..), deleteWhere, insert,
                                              runSqlPool)
import           Database.Persist.Sql        (ConnectionPool)
import           Database.Persist.Types      (Filter)
import           Lib                         (App, AppT (runAppT), initialize,
                                              shutdownApp)
import           Network.HTTP.Types.Method
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Network.Wai                 (Application)

import qualified Data.Text                   as T
import           Db
import           Logger                      (defaultLogEnv)
import           Schema
import qualified TestFeatures

main :: IO ()
main = do
  bracket
    webSetup
    (\(cfg, _) -> teardown cfg)
    (\(_, app) -> hspec $ webSpec app)
  hspec TestFeatures.spec

-- runAppToIO :: Config -> App a -> IO a
-- runAppToIO config app = do
--     result <- runExceptT $ runReaderT (runAppT app) config
--     case result of
--         Left err -> throwIO err
--         Right a  -> return a

webSetup :: IO (Config, Application)
webSetup = do
    env <- defaultLogEnv
    pool <- makePool Test env
    let cfg = Config { configPool = pool
                     , configEnv = Test
                     , configLogEnv = env
                     , configPort = 8081
                     }
    migrateDb pool
    app <- initialize cfg
    return (cfg, app)
  where
    migrateDb :: ConnectionPool -> IO ()
    migrateDb = runSqlPool doMigrations

teardown :: Config -> IO ()
teardown cfg = do
    cleanDb $ configPool cfg
    shutdownApp cfg
  where
    cleanDb :: ConnectionPool -> IO ()
    cleanDb = deleteAllUsers
    deleteAllUsers :: ConnectionPool -> IO ()
    deleteAllUsers pool =
        flip runSqlPool pool $ deleteWhere ([] :: [Filter User])

webSpec :: Application -> Spec
webSpec app =
     beforeAll (pure app) $
        describe "/users" $ do

            it "inserts a new user" $ do
              let postBody = [json|{userFirstName: "Isaac",
                                    userLastName: "Newton"}|]
              let postHeader = [("Content-Type", "application/json")]
              request methodPost "/users" postHeader postBody
              `shouldRespondWith` 200

            it "responds with [User]" $ do
                let users = [json|[{userFirstName: "Isaac",
                                   userLastName: "Newton"}]|]
                get "/users" `shouldRespondWith` users
