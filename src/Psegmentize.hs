module Psegmentize where

import Types
import Schema

data SegmentInventory = SegmentInventory
  { consonants :: PhonemeMatrix
  , vowels :: PhonemeMatrix
  , tones :: PhonemeMatrix
  , syllabicConsonants :: PhonemeMatrix
  , diphthongs :: PhonemeMatrix
  , unknowns :: PhonemeMatrix
  }

getConsonants :: [SegmentInfo] -> PhonemeMatrix
getConsonants segments =
  undefined

