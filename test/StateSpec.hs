module StateSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import State

spec :: Spec
spec = do
  describe "mkState" $ do
    it "should create a State with the filename" $ do
       mkState "testFilename" `shouldBe` State "testFilename"
