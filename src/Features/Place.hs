{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Features.Place
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

import           Data.Text  (Text)
import           Features
import           Schema

-- Features

data CorePOA = Labial
             | Labiodental
             | Linguolabial
             | Dental
             | Alveolar
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

isLabial = allp [ isVal "+" segmentLabial
                , isVal "-" segmentRound
                , isVal "-" segmentLabiodental
                , isVal "-" segmentCoronal
                , isVal "-" segmentDorsal
                ]

isPalatalizedLabial = allp [ isVal "+" segmentLabial
                           , isVal "-" segmentRound
                           , isVal "-" segmentLabiodental
                           , isVal "-" segmentCoronal
                           , isVal "+" segmentDorsal
                           , isVal "+" segmentFront
                           , isVal "-" segmentBack
                           ]

isRoundedLabial = allp [ isVal "+" segmentLabial
                       , isVal "+" segmentRound
                       , isVal "-" segmentLabiodental
                       , isVal "-" segmentCoronal
                       , isVal "-" segmentDorsal
                       , isVal "-" segmentEpilaryngealSource
                       ]

isRhotacizedLabial = allp [ isVal "+" segmentLabial
                          , isVal "-" segmentRound
                          , isVal "+" segmentCoronal
                          , isVal "+" segmentAnterior
                          , isVal "+" segmentDistributed
                          , isVal "+" segmentDorsal
                          ]

isLabiodental = allp [ isVal "-" segmentRound
                     , isVal "+" segmentLabiodental
                     , isVal "-" segmentDorsal
                     ]

isPalatalizedLabiodental = allp [ isVal "-" segmentRound
                                , isVal "+" segmentLabiodental
                                , isVal "+" segmentDorsal
                                , isVal "+" segmentFront
                                , isVal "-" segmentBack
                                ]

isRoundedLabiodental = allp [ isVal "+" segmentRound
                            , isVal "+" segmentLabiodental
                            ]

isLinguolabial = allp [ isVal "+" segmentLabial
                      , isNull segmentRound
                      , isNull segmentLabiodental
                      , isVal "+" segmentCoronal
                      , isVal "+" segmentAnterior
                      , isVal "-" segmentDorsal
                      ]

isDental = allp [ isVal "-" segmentLabial
                , isVal "+" segmentCoronal
                , isVal "+" segmentAnterior
                , isVal "+" segmentDistributed
                , isVal "-" segmentDorsal
                ]

isRoundedDental = allp [ isVal "+" segmentLabial
                       , isVal "+" segmentRound
                       , isVal "+" segmentCoronal
                       , isVal "+" segmentAnterior
                       , isVal "+" segmentDistributed
                       , isVal "-" segmentDorsal
                       ]

isVelarizedDental = allp [ isVal "-" segmentLabial
                         , isVal "+" segmentCoronal
                         , isVal "+" segmentAnterior
                         , isVal "+" segmentDistributed
                         , isVal "+" segmentDorsal
                         , isVal "-" segmentFront
                         , isVal "-" segmentBack
                         ]

isAlveolar = allp [ isVal "-" segmentLabial
                  , isVal "+" segmentCoronal
                  , isVal "+" segmentAnterior
                  , isVal "-" segmentDistributed
                    ||| isNull segmentDistributed
                  , isVal "-" segmentDorsal
                  ]

isRoundedAlveolar = allp [ isVal "+" segmentRound
                         , isVal "+" segmentCoronal
                         , isVal "+" segmentAnterior
                         , isVal "-" segmentDorsal
                         ]

isPalatalizedAlveolar = allp [ isVal "-" segmentLabial
                             , isVal "+" segmentCoronal
                             , isVal "+" segmentAnterior
                             , isVal "-" segmentDistributed
                             , isVal "+" segmentDorsal
                             , isVal "+" segmentFront
                             , isVal "-" segmentBack
                             ]

isRoundedPalatalizedAlveolar = allp [ isVal "+" segmentLabial
                                    , isVal "+" segmentRound
                                    , isVal "+" segmentCoronal
                                    , isVal "+" segmentAnterior
                                    , isVal "-" segmentDistributed
                                    , isVal "+" segmentDorsal
                                    , isVal "+" segmentFront
                                    , isVal "-" segmentBack
                                    ]

isVelarizedAlveolar = allp [ isVal "-" segmentLabial
                           , isVal "+" segmentCoronal
                           , isVal "+" segmentAnterior
                           , isVal "-" segmentDistributed
                           , isVal "+" segmentDorsal
                           , isVal "-" segmentFront
                           , isVal "-" segmentBack
                           ]

isRetroflex = allp [ isVal "-" segmentLabial
                   , isVal "+" segmentCoronal
                   , isVal "-" segmentAnterior
                     ||| isNull segmentAnterior
                   , isVal "-" segmentDorsal
                   , isVal "-" segmentDistributed
                   ]

isRoundedRetroflex = allp [ isVal "+" segmentLabial
                          , isVal "+" segmentRound
                          , isVal "+" segmentCoronal
                          , isVal "-" segmentAnterior
                            ||| isNull segmentAnterior
                          , isVal "-" segmentDorsal
                          , isVal "-" segmentDistributed
                          ]

isPalatalizedRetroflex = allp [ isVal "-" segmentLabial
                              , isVal "+" segmentCoronal
                              , isVal "-" segmentAnterior
                              , isVal "-" segmentDistributed
                              , isVal "+" segmentDorsal
                              , isVal "+" segmentFront
                              , isVal "-" segmentBack
                              ]

isAlveolopalatal = allp [ isVal "-" segmentLabial
                        , isVal "+" segmentCoronal
                        , isVal "-" segmentAnterior
                        , isVal "+" segmentDistributed
                        , isVal "-" segmentDorsal
                        ]

isRoundedAlveolopalatal = allp [ isVal "+" segmentLabial
                               , isVal "+" segmentRound
                               , isVal "+" segmentCoronal
                               , isVal "-" segmentAnterior
                               , isVal "+" segmentDistributed
                               , isVal "-" segmentDorsal
                               ]

isPalatalizedAlveolopalatal = allp [ isVal "-" segmentLabial
                                   , isVal "+" segmentCoronal
                                   , isVal "-" segmentAnterior
                                   , isVal "+" segmentDistributed
                                   , isVal "+" segmentDorsal
                                   , isVal "+" segmentFront
                                   , isVal "-" segmentBack
                                   , isVal "+" segmentStrident
                                   ]


isVelarizedAlveolopalatal = allp [ isVal "-" segmentLabial
                                 , isVal "+" segmentCoronal
                                 , isVal "-" segmentAnterior
                                 , isVal "+" segmentDistributed
                                 , isVal "+" segmentDorsal
                                 , isVal "-" segmentFront
                                 , isVal "-" segmentBack
                                 ]

isCompressedAlveolopalatal = allp [ isVal "+" segmentLabial
                                  , isVal "-" segmentRound
                                  , isVal "+" segmentCoronal
                                  , isVal "-" segmentAnterior
                                  , isVal "+" segmentDistributed
                                  , isVal "-" segmentDorsal
                                  ]

isPalatoalveolar = allp [ isVal "-" segmentLabial
                        , isVal "+" segmentCoronal
                        , isVal "+" segmentAnterior
                        , isVal "+" segmentDistributed
                        , isVal "+" segmentDorsal
                        , isVal "+" segmentFront
                        , isVal "-" segmentBack
                        ]

isRoundedPalatoalveolar = allp [ isVal "+" segmentLabial
                               , isVal "+" segmentRound
                               , isVal "+" segmentCoronal
                               , isVal "+" segmentAnterior
                               , isVal "+" segmentDistributed
                               , isVal "+" segmentDorsal
                               ]

isRoundedPalatalizedPalatoalveolar = allp [ isVal "+" segmentLabial
                                          , isVal "+" segmentRound
                                          , isVal "+" segmentCoronal
                                          , isVal "+" segmentAnterior
                                          , isVal "+" segmentDistributed
                                          , isVal "+" segmentDorsal
                                          , isVal "+" segmentFront
                                          , isVal "-" segmentBack
                                          ]

isPalatal = allp [ isVal "-" segmentLabial
                 , isVal "+" segmentCoronal
                 , isVal "-" segmentAnterior
                 , isVal "+" segmentDistributed
                 , isVal "+" segmentDorsal
                 , isVal "+" segmentHigh
                 , isVal "-" segmentLow
                 , isVal "+" segmentFront
                 , isVal "-" segmentBack
                 ]
            |||
            allp [ isVal "-" segmentLabial
                 , isVal "-" segmentCoronal
                 , isVal "+" segmentDorsal
                 , isVal "+" segmentHigh
                 , isVal "-" segmentLow
                 , isVal "+" segmentFront
                 , isVal "-" segmentBack
                 ]

isRoundedPalatal = allp [ isVal "+" segmentLabial
                        , isVal "+" segmentRound
                        , isVal "+" segmentCoronal
                        , isVal "-" segmentAnterior
                        , isVal "+" segmentDistributed
                        , isVal "+" segmentDorsal
                        , isVal "+" segmentHigh
                        , isVal "-" segmentLow
                        , isVal "+" segmentFront
                        , isVal "-" segmentBack
                        ]
                   |||
                   allp [ isVal "+" segmentLabial
                        , isVal "+" segmentRound
                        , isVal "-" segmentCoronal
                        , isVal "+" segmentDorsal
                        , isVal "+" segmentHigh
                        , isVal "-" segmentLow
                        , isVal "+" segmentFront
                        , isVal "-" segmentBack
                        ]

isSje = allp [ isVal "-" segmentLabial
             , isVal "+" segmentCoronal
             , isVal "-" segmentAnterior
             , isVal "+" segmentDistributed
             , isVal "+" segmentDorsal
             , isVal "+" segmentHigh
             , isVal "-" segmentLow
             , isVal "-" segmentFront
             , isVal "+" segmentBack
             ]

isVelar = allp [ isVal "-" segmentLabial
               , isVal "-" segmentCoronal
               , isVal "+" segmentDorsal
               , isVal "+" segmentHigh
               , isVal "-" segmentLow
               , isVal "-" segmentFront
               , isVal "-" segmentBack
                 ||| isVal "+" segmentBack
               ]

isRoundedVelar = allp [ isVal "+" segmentLabial
                      , isVal "+" segmentRound
                      , isVal "-" segmentCoronal
                      , isVal "+" segmentDorsal
                      , isVal "+" segmentHigh
                      , isVal "-" segmentLow
                      , isVal "-" segmentFront
                      , isVal "-" segmentBack -- most rounded velars
                        ||| isVal "+" segmentBack -- w and friends
                      ]

isLabialAlveolar = allp [ isVal "+" segmentLabial
                        , isVal "-" segmentRound
                        , isVal "-" segmentLabiodental
                        , isVal "+" segmentCoronal
                        , isVal "+" segmentAnterior
                        , isVal "-" segmentDistributed
                        , isVal "+" segmentDorsal
                        ]

isLabialVelar = allp [ isVal "+" segmentLabial
                     , isVal "-" segmentCoronal
                     , isVal "+" segmentDorsal
                     , isVal "-" segmentRound
                     ]

isPalatalizedLabialVelar = const False

isRoundedLabialVelar = const False

isUvular = allp [ isVal "-" segmentLabial
                , isVal "+" segmentDorsal
                , isVal "-" segmentHigh
                , isVal "-" segmentLow
                ]

isPalatalizedUvular = const False

isRoundedUvular = allp [ isVal "+" segmentLabial
                       , isVal "+" segmentRound
                       , isVal "+" segmentDorsal
                       , isVal "-" segmentHigh
                       , isVal "-" segmentLow
                       ]

isPharyngeal = allp [ isVal "-" segmentLabial
                    , isVal "+" segmentDorsal
                    , isVal "-" segmentHigh
                    , isVal "+" segmentLow
                    ]

isRoundedPharyngeal = allp [ isVal "+" segmentLabial
                           , isVal "+" segmentRound
                           , isVal "+" segmentDorsal
                           , isVal "-" segmentHigh
                           , isVal "+" segmentLow
                           ]

isEpiglottal = allp [ isVal "-" segmentLabial
                    , isVal "+" segmentEpilaryngealSource
                    ]

isRoundedEpiglottal = allp [ isVal "+" segmentLabial
                           , isVal "+" segmentRound
                           , isVal "+" segmentEpilaryngealSource
                           ]

isGlottal = const False

getPOA :: Segment -> POA
getPOA seg =
  case seg of

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
    | isRetroflex s -> POA Alveolar Nothing Nothing True
    | isRoundedRetroflex s -> POA Alveolar (Just Rounded) Nothing True
    | isPalatalizedRetroflex s -> POA Alveolar Nothing (Just Palatalized) True

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
