{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Db where

import           Control.Monad.Reader        (MonadIO, MonadReader, asks,
                                              liftIO)
import           Database.Persist.Postgresql

import           Config                      (Config (..))
import           Data.Text                   (Text)
import           Schema


doMigrations :: SqlPersistT IO ()
doMigrations = do
  printMigration migrateAll
  runMigration migrateAll

runDb :: (MonadIO m, MonadReader Config m) => SqlPersistT IO b -> m b
runDb query = do
  conn <- asks configPool
  liftIO $ runSqlPool query conn

initDb :: MonadIO m => ConnectionPool -> m ()
initDb pool = liftIO $ runSqlPool doMigrations pool

closeDb :: SqlBackend -> IO ()
closeDb = close'
