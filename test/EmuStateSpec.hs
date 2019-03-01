{-# LANGUAGE OverloadedStrings #-}

module EmuStateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import EmuState

spec :: Spec
spec =
  describe "EmuState" $ do
    let state = EmuState {fileName = "testFilename", memory = "ByteString"}
    it "should create a State with fileName" $ fileName state `shouldBe` "testFilename"
    it "should create a State with memory" $ memory state `shouldBe` "ByteString"
