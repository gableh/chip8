{-# LANGUAGE OverloadedStrings #-}

module UtilsSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Utils

spec :: Spec
spec = describe "readHex" $ it "should read a hex string and return a Num type" $ fromHex "FF" `shouldBe` 255
