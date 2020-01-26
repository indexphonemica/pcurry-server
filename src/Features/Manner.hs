{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Features.Manner
Description : Contains functions to generate sensible features from Phoible's
Copyright   : (c) HallowXIII 2019
License     : MIT
Maintainer  : zephyrnox@gmail.com
Stability   : experimental
Portability : unknown

This module contains features to deal with the highly idiosyncratic and
severely broken featuralization system used by PHOIBLE (and, currently, IPHON).

Most of these functions are internal and not exported. Only the functions
returning the relevant information directly are exported in order to save the
user the headache of writing an enormous case statement themselves.
-}

module Features.Manner
where

import           Data.Text (Text)
import           Features
import           Schema

data CoreMOA =
  Stop
  | Affricate
  | Fricative
  | Resonant
  | Trill
  | Tap
  deriving (Eq, Ord, Show)

data Nasality = Full | Release
  deriving (Eq, Ord, Show)

data Centrality = Central | Lateral
  deriving (Eq, Ord, Show)

data MOA = MOA { coreMOA    :: CoreMOA
               , nasality   :: Maybe Nasality
               , centrality :: Maybe Centrality
               }
  deriving (Eq, Ord, Show)


isLateralAffricate =
  allp [ isVal "+" segmentConsonantal
       , isVal "-" segmentSonorant
       , isVal "-,+" segmentContinuant
       , isVal "-,+" segmentDelayedRelease
       , isVal "-" segmentApproximant
       , isVal "-" segmentTap
       , isVal "-" segmentTrill
       , isVal "-" segmentNasal
       , isVal "-,+" segmentLateralis
       ]
  |||
  allp [ isVal "+" segmentConsonantal
       , isVal "-,+" segmentSonorant
       , isVal "-,+" segmentContinuant
       , isVal "-" segmentDelayedRelease
       , isVal "-,+" segmentApproximant
       , isVal "-" segmentTap
       , isVal "-" segmentTrill
       , isVal "-" segmentNasal
       , isVal "-,+" segmentLateralis
       ]

isPlosive = allp [ isVal "+" segmentConsonantal
                 , isVal "-" segmentSonorant
                   ||| isVal "+,-" segmentSonorant
                 , isVal "-" segmentContinuant
                 , isVal "-" segmentDelayedRelease
                 , isVal "-" segmentApproximant
                 , isVal "-" segmentTap
                 , isVal "-" segmentTrill
                 , isVal "-" segmentNasal
                   ||| isVal "+,-" segmentNasal
                 , isVal "-" segmentLateralis
                 ]

isLaterallyReleasedPlosive = allp [ isVal "+" segmentConsonantal
                                  , isVal "-" segmentSonorant
                                  , isVal "-" segmentContinuant
                                  , isVal "-" segmentDelayedRelease
                                  , isVal "-" segmentApproximant
                                  , isVal "-" segmentTap
                                  , isVal "-" segmentTrill
                                  , isVal "-" segmentNasal
                                  , isVal "+" segmentLateralis
                                  ]

isNasalizedPlosive = allp [ isVal "+" segmentConsonantal
                          , isVal "-" segmentSonorant
                          , isVal "-" segmentContinuant
                          , isVal "-" segmentDelayedRelease
                          , isVal "-" segmentApproximant
                          , isVal "-" segmentTap
                          , isVal "-" segmentTrill
                          , isVal "+" segmentNasal
                          , isVal "-" segmentLateralis
                          ]
