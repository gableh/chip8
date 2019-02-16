{-# LANGUAGE OverloadedStrings #-}

module EmuStateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import EmuState

spec :: Spec
spec = do
  describe "EmuState" $ do
    let state = EmuState {
      fileName = "testFilename",
      memory = "ByteString"
    }
    it "should create a State with fileName" $ do
       fileName state `shouldBe` "testFilename"
    it "should create a State with memory" $ do
       memory state `shouldBe` "ByteString"