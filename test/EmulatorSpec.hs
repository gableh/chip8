{-# LANGUAGE OverloadedStrings #-}

module EmulatorSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed         as U
import Emulator
-- import Test.QuickCheck

-- import Emulator

spec :: Spec
spec = do
  describe "runCPU" $ do
    describe "00E0" $ do
      it "should clear the display" $ do
        runCPU "00E0" (U.replicate 10 1) `shouldBe` (U.replicate 10 0)