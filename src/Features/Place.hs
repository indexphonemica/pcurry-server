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

module Features.Place
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
             | Unknown
             deriving (Eq, Ord, Show)

data LabialState = Compressed | Rounded
  deriving (Eq, Ord, Show)

data DorsalState = Palatalized | Velarized
  deriving (Eq, Ord, Show)

data POA = POA { corePOA     :: CorePOA
               , labialState :: Maybe LabialState
               , dorsalState :: Maybe DorsalState
               , retroflexed :: Bool
               }
  deriving (Eq, Ord, Show)

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

isRhotacizedLabial = allp [ isPositive segmentLabial
                          , isNegative segmentRound
                          , isPositive segmentCoronal
                          , isPositive segmentAnterior
                          , isPositive segmentDistributed
                          , isPositive segmentDorsal
                          ]

isLabiodental = allp [ isNegative segmentRound
                     , isPositive segmentLabiodental
                     , isNegative segmentDorsal
                     ]

isPalatalizedLabiodental = allp [ isNegative segmentRound
                                , isPositive segmentLabiodental
                                , isPositive segmentDorsal
                                , isPositive segmentFront
                                , isNegative segmentBack
                                ]

isRoundedLabiodental = allp [ isPositive segmentRound
                            , isPositive segmentLabiodental
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

isRoundedPharyngeal = allp [ isPositive segmentLabial
                           , isPositive segmentRound
                           , isPositive segmentDorsal
                           , isNegative segmentHigh
                           , isPositive segmentLow
                           ]

isEpiglottal = allp [ isNegative segmentLabial
                    , isPositive segmentEpilaryngealSource
                    ]

isRoundedEpiglottal = allp [ isPositive segmentLabial
                           , isPositive segmentRound
                           , isPositive segmentEpilaryngealSource
                           ]

isGlottal = const False

getPOA :: Segment -> POA
getPOA seg = case seg of
  -- Labials
  s | isLabial s -> POA Labial Nothing Nothing False
    | isRoundedLabial s -> POA Labial (Just Rounded) Nothing False
    | isPalatalizedLabial s -> POA Labial Nothing (Just Palatalized) False
    | isRhotacizedLabial s -> POA Labial Nothing Nothing True
  -- Labiodentals
    | isLabiodental s -> POA Labiodental Nothing Nothing False
    | isPalatalizedLabiodental s -> POA Labiodental Nothing (Just Palatalized) False
    | isRoundedLabiodental s -> POA Labiodental (Just Rounded) Nothing False
  -- Linguolabials
    | isLinguolabial s -> POA Linguolabial Nothing Nothing False
  -- Dentals
    | isDental s -> POA Dental Nothing Nothing False
    | isRoundedDental s -> POA Dental (Just Rounded) Nothing False
    | isVelarizedDental s -> POA Dental Nothing (Just Velarized) False
  -- Alveolars
    | isAlveolar s -> POA Alveolar Nothing Nothing False
    | isRoundedAlveolar s -> POA Alveolar (Just Rounded) Nothing False
    | isPalatalizedAlveolar s -> POA Alveolar Nothing (Just Palatalized) False
    | isRoundedPalatalizedAlveolar s -> POA Alveolar (Just Rounded) (Just Palatalized) False
    | isVelarizedAlveolar s -> POA Alveolar Nothing (Just Velarized) False
  -- Retroflexes
    | isRetroflex s -> POA Retroflex Nothing Nothing False
    | isRoundedRetroflex s -> POA Retroflex (Just Rounded) Nothing False
    | isPalatalizedRetroflex s -> POA Retroflex Nothing (Just Palatalized) False
  -- Alveolopalatals
    | isAlveolopalatal s -> POA Alveolopalatal Nothing Nothing False
    | isRoundedAlveolopalatal s -> POA Alveolopalatal (Just Rounded) Nothing False
    | isPalatalizedAlveolopalatal s -> POA Alveolopalatal Nothing (Just Palatalized) False
    | isVelarizedAlveolopalatal s -> POA Alveolopalatal Nothing (Just Velarized) False
    | isCompressedAlveolopalatal s -> POA Alveolopalatal (Just Compressed) Nothing False
  -- Palatoalveolars
    | isPalatoalveolar s -> POA Palatoalveolar Nothing Nothing False
    | isRoundedPalatoalveolar s -> POA Palatoalveolar (Just Rounded) Nothing False
    | isRoundedPalatalizedPalatoalveolar s -> POA Palatoalveolar (Just Rounded) (Just Palatalized) False
  -- Palatals
    | isPalatal s -> POA Palatal Nothing Nothing False
    | isRoundedPalatal s -> POA Palatal (Just Rounded) Nothing False
  -- Sje
    | isSje s -> POA Sje Nothing Nothing False
  -- Velars
    | isVelar s -> POA Velar Nothing Nothing False
    | isRoundedVelar s -> POA Velar (Just Rounded) Nothing False
  -- Labial-Alveolars and Labial-Velars
    | isLabialAlveolar s -> POA LabialAlveolar Nothing Nothing False
    | isLabialVelar s -> POA LabialVelar Nothing Nothing False
    | isRoundedLabialVelar s -> POA LabialVelar (Just Rounded) Nothing False
    | isPalatalizedLabialVelar s -> POA LabialVelar Nothing (Just Palatalized) False
  -- Uvulars
    | isUvular s -> POA Uvular Nothing Nothing False
    | isRoundedUvular s -> POA Uvular (Just Rounded) Nothing False
    | isPalatalizedUvular s -> POA Uvular Nothing (Just Palatalized) False
  -- Pharyngeals
    | isPharyngeal s -> POA Pharyngeal Nothing Nothing False
    | isRoundedPharyngeal s -> POA Pharyngeal (Just Rounded) Nothing False
  -- Epiglottals
    | isEpiglottal s -> POA Epiglottal Nothing Nothing False
    | isRoundedEpiglottal s -> POA Epiglottal (Just Rounded) Nothing False
  -- Glottals
    | isGlottal s -> POA Glottal Nothing Nothing False
  _ -> POA Unknown Nothing Nothing False
