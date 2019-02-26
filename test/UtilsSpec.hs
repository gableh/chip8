{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Utils

spec :: Spec
spec = do
  describe "readHex" $ do
    it "should read a hex string and return a Num type" $ do
      fromHex "FF" `shouldBe` 255