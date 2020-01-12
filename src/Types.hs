module Types where

import           Data.Text (Text)
import           Schema

type Realization = Text
type Phoneme = Text
type SegmentInfo = (Segment, Int, Bool)
type AllophoneInfo =
  (Bool, Text, Text, Maybe Phoneme, Maybe Realization, Segment)
type DoculectInfo =
  (Doculect, Language, [SegmentInfo], [AllophoneInfo])
