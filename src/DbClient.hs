{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module DbClient where

import           Control.Monad.Reader      (MonadIO, lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Text                 (Text)
import           Database.Esqueleto
import           Schema
import           Types

doculectQuery :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity Doculect))
doculectQuery iid = getBy $ UniqueIID iid

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
  SqlPersistT m (Maybe (Entity Language))
doculectLanguageQuery doc =
  getBy $ UniqueGlottocode $ doculectGlottocode . entityVal $ doc

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

doculectInfoQuery :: MonadIO m =>
  Text -> SqlPersistT m
  (Maybe
    (Entity Doculect, Entity Language,
      [(Entity Segment, Value Int, Value Bool)],
      [(Value Bool, Value Text, Value Text, Value (Maybe Phoneme),
         Value (Maybe Realization), Entity Segment)]))
doculectInfoQuery iid = runMaybeT $ do
  doc <- MaybeT $ doculectQuery iid
  lang <- MaybeT $ doculectLanguageQuery doc
  segments <- lift $ doculectSegmentsQuery doc
  allophones <- lift $ doculectAllophoneQuery doc
  return (doc, lang, segments, allophones)

getDoculectInfo :: MonadIO m => Text -> SqlPersistT m (Maybe DoculectInfo)
getDoculectInfo iid =
  do
    rawDoculectInfo <- doculectInfoQuery iid
    return $ fmap unSql rawDoculectInfo
  where
    unSql (ed, el, sis, ais) =
      (entityVal ed, entityVal el, map sInfoFromSql sis, map aInfoFromSql ais)
    sInfoFromSql (es, vi, vb) = (entityVal es, unValue vi, unValue vb)
    aInfoFromSql (vb, vt, vt', vmp, vmr, es) =
      (unValue vb, unValue vt, unValue vt',
       unValue vmp, unValue vmr, entityVal es)
