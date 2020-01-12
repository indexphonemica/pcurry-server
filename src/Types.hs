{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics
import           Schema

type Realization = Text
type Phoneme = Text
type SegmentInfo = (Segment, Int, Bool)
type AllophoneInfo =
  (Bool, Text, Text, Maybe Phoneme, Maybe Realization, Segment)
type DoculectInfo =
  (Doculect, Language, [SegmentInfo], [AllophoneInfo])

data InventorySegment = InventorySegment
  { segment  :: Text
  , marginal :: Bool
  , loan     :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON InventorySegment

type PhonemeMatrix = [[InventorySegment]]
