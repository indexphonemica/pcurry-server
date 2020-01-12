{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module DbClient where

import           Config
import           Control.Monad.Reader (MonadIO, MonadReader, liftIO)
import           Data.Text            (Text)
import           Database.Esqueleto
import           Db
import           Schema

type Realization = Text
type Phoneme = Text

doculectQuery :: MonadIO m => Text -> SqlPersistT m [Entity Doculect]
doculectQuery iid = select $
  from $ \doculects -> do
  where_ (doculects ^. DoculectInventoryId ==. val iid)
  return doculects

doculectSegmentsQuery :: MonadIO m =>
  Entity Doculect ->
  SqlPersistT m [(Entity Segment, Value Int, Value Bool)]
doculectSegmentsQuery doc = select $
  from $ \(d `InnerJoin` ds `InnerJoin` s) -> do
  on (d ^. DoculectId ==. ds ^. DoculectSegmentDoculectId)
  on (ds ^. DoculectSegmentSegmentId ==. s ^. SegmentId)
  where_ (d ^. DoculectId ==. val (entityKey doc))
  return (s, ds ^. DoculectSegmentMarginal, ds ^. DoculectSegmentLoan)

doculectLanguageQuery :: MonadIO m =>
  Entity Doculect ->
  SqlPersistT m [(Entity Doculect, Entity Language)]
doculectLanguageQuery doc = select $
  from $ \(doculects `InnerJoin` languages) -> do
  on (doculects ^. DoculectGlottocode ==. languages ^. LanguageGlottocode)
  where_ (doculects ^. DoculectId ==. val (entityKey doc))
  return (doculects, languages)

doculectAllophoneQuery :: MonadIO m =>
  Entity Doculect ->
  SqlPersistT m [(Value Bool, Value Text, Value Text,
                  Value (Maybe Phoneme), Value (Maybe Realization),
                  Entity Segment)]
doculectAllophoneQuery doc = select $
  from $
  \(a `InnerJoin` ds `InnerJoin` d `InnerJoin` p `InnerJoin` r) -> do
  on $ a ^. AllophoneDoculectSegmentId ==. ds ^. DoculectSegmentId
  on $ ds ^. DoculectSegmentSegmentId ==. p ^. SegmentId
  on $ ds ^. DoculectSegmentDoculectId ==. d ^. DoculectId
  on $ a ^. AllophoneAllophoneId ==. r ^. SegmentId
  where_ (d ^. DoculectId ==. val (entityKey doc))
  return (a ^. AllophoneVariation
         , a ^. AllophoneCompound
         , a ^. AllophoneEnvironment
         , p ^. SegmentPhoneme
         , r ^. SegmentPhoneme
         , p)

doculect :: (MonadIO m, MonadReader Config m) =>
  Text -> m (Maybe Doculect)
doculect iid = do
  candidates <- runDb $ doculectQuery iid
  case candidates of
    []  -> return Nothing
    e:_ -> return $ Just $ entityVal e
