{-# LANGUAGE OverloadedStrings #-}

module TestFeatures (spec) where

import           Test.Hspec
import           Features.Place
import           Schema

phonemeP :: Segment
phonemeP = Segment
  (Just "p")
  Nothing
  (Just "consonant")
  Nothing
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "+")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "+")
  (Just "-")
  (Just "-")
  (Just "-")
  Nothing
  Nothing
  Nothing
  (Just "-")
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")
  (Just "-")

spec :: Spec
spec = describe "getPOA" $ do
  it "correctly recognizes 'p' as labial" $
    getPOA phonemeP `shouldBe` POA Labial Nothing Nothing False
