{-# LANGUAGE OverloadedStrings #-}

module Features

where

import           Data.Maybe (isNothing)
import           Data.Text  (Text)
import           Schema


-- | A phonemic feature of some description.
-- | The field @test@ contains a function that can be used to check if a given
-- | 'Segment' has the feature or not.
data Feature = Feature
  { name  :: Text
  , order :: Float
  , test  :: Segment -> Bool
  }

isVal :: Text -> (Segment -> Maybe Text) -> Segment -> Bool
isVal t f s = maybe False (t ==) (f s)

isNull :: (Segment -> Maybe Text) -> Segment -> Bool
isNull f s = isNothing $ f s

isPositive :: (Segment -> Maybe Text) -> Segment -> Bool
isPositive = isVal "+"

isNegative :: (Segment -> Maybe Text) -> Segment -> Bool
isNegative = isVal "-"

-- | Applies a lifted function to a normal value and returns a lifted result.
apF :: Functor f => f (a -> b) -> a -> f b
apF f v = fmap (\g -> g v) f

-- | Checks whether a list of predicates on a type @a@ holds for a given
-- | value of type @a@.
allp :: [a -> Bool] -> a -> Bool
allp preds val = and $ preds `apF` val

-- | Function-level or.
infixl 4 |||
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f g v = f v || g v
