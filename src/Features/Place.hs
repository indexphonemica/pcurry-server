{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Features
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

module Features.Consonants
where

import           Data.Text (Text)
import           Features
import           Schema

-- Features

data CorePOA = Labial
             | Labiodental
             | Linguolabial
             | Dental
             | Alveolar
             | Retroflex
             | Alveolopalatal
             | Palatoalveolar
             | Palatal
             | Sje
             | Velar
             | LabialAlveolar
             | LabialVelar
             | Uvular
             | Pharyngeal
             | Epiglottal
             | Glottal
             deriving (Eq, Ord, Show)

data LabialState = Compressed | Rounded

data DorsalState = Palatalized | Velarized

data POA = POA { corePOA     :: CorePOA
               , labialState :: Maybe LabialState
               , dorsalState :: Maybe DorsalState
               , retroflexed :: Bool
               }

isLabial = allp [ isPositive segmentLabial
                , isNegative segmentRound
                , isNegative segmentLabiodental
                , isNegative segmentCoronal
                , isNegative segmentDorsal
                ]

isPalatalizedLabial = allp [ isPositive segmentLabial
                           , isNegative segmentRound
                           , isNegative segmentLabiodental
                           , isNegative segmentCoronal
                           , isPositive segmentDorsal
                           , isPositive segmentFront
                           , isNegative segmentBack
                           ]

isRoundedLabial = allp [ isPositive segmentLabial
                       , isPositive segmentRound
                       , isNegative segmentLabiodental
                       , isNegative segmentCoronal
                       , isNegative segmentDorsal
                       , isNegative segmentEpilaryngealSource
                       ]

isLinguolabial = allp [ isPositive segmentLabial
                      , isNull segmentRound
                      , isNull segmentLabiodental
                      , isPositive segmentCoronal
                      , isPositive segmentAnterior
                      , isNegative segmentDorsal
                      ]

isDental = allp [ isNegative segmentLabial
                , isPositive segmentCoronal
                , isPositive segmentAnterior
                , isPositive segmentDistributed
                , isNegative segmentDorsal
                ]

isRoundedDental = allp [ isPositive segmentLabial
                       , isPositive segmentRound
                       , isPositive segmentCoronal
                       , isPositive segmentAnterior
                       , isPositive segmentDistributed
                       , isNegative segmentDorsal
                       ]

isVelarizedDental = allp [ isNegative segmentLabial
                         , isPositive segmentCoronal
                         , isPositive segmentAnterior
                         , isPositive segmentDistributed
                         , isPositive segmentDorsal
                         , isNegative segmentFront
                         , isNegative segmentBack
                         ]

isAlveolar = allp [ isNegative segmentLabial
                  , isPositive segmentCoronal
                  , isPositive segmentAnterior
                  , isNegative segmentDistributed
                    ||| isNull segmentDistributed
                  , isNegative segmentDorsal
                  ]

isRoundedAlveolar = allp [ isPositive segmentRound
                         , isPositive segmentCoronal
                         , isPositive segmentAnterior
                         , isNegative segmentDorsal
                         ]

isPalatalizedAlveolar = allp [ isNegative segmentLabial
                             , isPositive segmentCoronal
                             , isPositive segmentAnterior
                             , isNegative segmentDistributed
                             , isPositive segmentDorsal
                             , isPositive segmentFront
                             , isNegative segmentBack
                             ]

isRoundedPalatalizedAlveolar = allp [ isPositive segmentLabial
                                    , isPositive segmentRound
                                    , isPositive segmentCoronal
                                    , isPositive segmentAnterior
                                    , isNegative segmentDistributed
                                    , isPositive segmentDorsal
                                    , isPositive segmentFront
                                    , isNegative segmentBack
                                    ]

isVelarizedAlveolar = allp [ isNegative segmentLabial
                           , isPositive segmentCoronal
                           , isPositive segmentAnterior
                           , isNegative segmentDistributed
                           , isPositive segmentDorsal
                           , isNegative segmentFront
                           , isNegative segmentBack
                           ]

isRetroflex = allp [ isNegative segmentLabial
                   , isPositive segmentCoronal
                   , isNegative segmentAnterior
                     ||| isNull segmentAnterior
                   , isNegative segmentDorsal
                   , isNegative segmentDistributed
                   ]

isRoundedRetroflex = allp [ isPositive segmentLabial
                          , isPositive segmentRound
                          , isPositive segmentCoronal
                          , isNegative segmentAnterior
                            ||| isNull segmentAnterior
                          , isNegative segmentDorsal
                          , isNegative segmentDistributed
                          ]

isPalatalizedRetroflex = allp [ isNegative segmentLabial
                              , isPositive segmentCoronal
                              , isNegative segmentAnterior
                              , isNegative segmentDistributed
                              , isPositive segmentDorsal
                              , isPositive segmentFront
                              , isNegative segmentBack
                              ]

isAlveolopalatal = allp [ isNegative segmentLabial
                        , isPositive segmentCoronal
                        , isNegative segmentAnterior
                        , isPositive segmentDistributed
                        , isNegative segmentDorsal
                        ]

isRoundedAlveolopalatal = allp [ isPositive segmentLabial
                               , isPositive segmentRound
                               , isPositive segmentCoronal
                               , isNegative segmentAnterior
                               , isPositive segmentDistributed
                               , isNegative segmentDorsal
                               ]

isPalatalizedAlveolopalatal = allp [ isNegative segmentLabial
                                   , isPositive segmentCoronal
                                   , isNegative segmentAnterior
                                   , isPositive segmentDistributed
                                   , isPositive segmentDorsal
                                   , isPositive segmentFront
                                   , isNegative segmentBack
                                   , isPositive segmentStrident
                                   ]


isVelarizedAlveolopalatal = allp [ isNegative segmentLabial
                                 , isPositive segmentCoronal
                                 , isNegative segmentAnterior
                                 , isPositive segmentDistributed
                                 , isPositive segmentDorsal
                                 , isNegative segmentFront
                                 , isNegative segmentBack
                                 ]

isCompressedAlveolopalatal = allp [ isPositive segmentLabial
                                  , isNegative segmentRound
                                  , isPositive segmentCoronal
                                  , isNegative segmentAnterior
                                  , isPositive segmentDistributed
                                  , isNegative segmentDorsal
                                  ]

isPalatoalveolar = allp [ isNegative segmentLabial
                        , isPositive segmentCoronal
                        , isPositive segmentAnterior
                        , isPositive segmentDistributed
                        , isPositive segmentDorsal
                        , isPositive segmentFront
                        , isNegative segmentBack
                        ]

isRoundedPalatoalveolar = allp [ isPositive segmentLabial
                               , isPositive segmentRound
                               , isPositive segmentCoronal
                               , isPositive segmentAnterior
                               , isPositive segmentDistributed
                               , isPositive segmentDorsal
                               ]

isRoundedPalatalizedPalatoalveolar = allp [ isPositive segmentLabial
                                          , isPositive segmentRound
                                          , isPositive segmentCoronal
                                          , isPositive segmentAnterior
                                          , isPositive segmentDistributed
                                          , isPositive segmentDorsal
                                          , isPositive segmentFront
                                          , isNegative segmentBack
                                          ]

isPalatal = allp [ isNegative segmentLabial
                 , isPositive segmentCoronal
                 , isNegative segmentAnterior
                 , isPositive segmentDistributed
                 , isPositive segmentDorsal
                 , isPositive segmentHigh
                 , isNegative segmentLow
                 , isPositive segmentFront
                 , isNegative segmentBack
                 ]
            |||
            allp [ isNegative segmentLabial
                 , isNegative segmentCoronal
                 , isPositive segmentDorsal
                 , isPositive segmentHigh
                 , isNegative segmentLow
                 , isPositive segmentFront
                 , isNegative segmentBack
                 ]

isRoundedPalatal = allp [ isPositive segmentLabial
                        , isPositive segmentRound
                        , isPositive segmentCoronal
                        , isNegative segmentAnterior
                        , isPositive segmentDistributed
                        , isPositive segmentDorsal
                        , isPositive segmentHigh
                        , isNegative segmentLow
                        , isPositive segmentFront
                        , isNegative segmentBack
                        ]
                   |||
                   allp [ isPositive segmentLabial
                        , isPositive segmentRound
                        , isNegative segmentCoronal
                        , isPositive segmentDorsal
                        , isPositive segmentHigh
                        , isNegative segmentLow
                        , isPositive segmentFront
                        , isNegative segmentBack
                        ]

isSje = allp [ isNegative segmentLabial
             , isPositive segmentCoronal
             , isNegative segmentAnterior
             , isPositive segmentDistributed
             , isPositive segmentDorsal
             , isPositive segmentHigh
             , isNegative segmentLow
             , isNegative segmentFront
             , isPositive segmentBack
             ]

isVelar = allp [ isNegative segmentLabial
               , isNegative segmentCoronal
               , isPositive segmentDorsal
               , isPositive segmentHigh
               , isNegative segmentLow
               , isNegative segmentFront
               , isNegative segmentBack
                 ||| isPositive segmentBack
               ]

isRoundedVelar = allp [ isPositive segmentLabial
                      , isPositive segmentRound
                      , isNegative segmentCoronal
                      , isPositive segmentDorsal
                      , isPositive segmentHigh
                      , isNegative segmentLow
                      , isNegative segmentFront
                      , isNegative segmentBack -- most rounded velars
                        ||| isPositive segmentBack -- w and friends
                      ]

isLabialAlveolar = allp [ isPositive segmentLabial
                        , isNegative segmentRound
                        , isNegative segmentLabiodental
                        , isPositive segmentCoronal
                        , isPositive segmentAnterior
                        , isNegative segmentDistributed
                        , isPositive segmentDorsal
                        ]

isLabialVelar = allp [ isPositive segmentLabial
                     , isNegative segmentCoronal
                     , isPositive segmentDorsal
                     , isNegative segmentRound
                     ]

isPalatalizedLabialVelar = const False

isRoundedLabialVelar = const False

isUvular = allp [ isNegative segmentLabial
                , isPositive segmentDorsal
                , isNegative segmentHigh
                , isNegative segmentLow
                ]

isPalatalizedUvular = const False

isRoundedUvular = allp [ isPositive segmentLabial
                       , isPositive segmentRound
                       , isPositive segmentDorsal
                       , isNegative segmentHigh
                       , isNegative segmentLow
                       ]

isPharyngeal = allp [ isNegative segmentLabial
                    , isPositive segmentDorsal
                    , isNegative segmentHigh
                    , isPositive segmentLow
                    ]

isEpiglottal = allp [ isNegative segmentLabial
                    , isPositive segmentEpilaryngealSource
                    ]

isGlottal = const False

getPOA :: Segment -> POA
getPOA seg = case seg of
  s | isLabial s -> POA Labial Nothing Nothing
    | isLabiodental s -> POA Labiodental Nothing Nothing
