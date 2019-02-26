{-# LANGUAGE OverloadedStrings #-}

module EmulatorSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed         as U
import Emulator
import EmuState
-- import Test.QuickCheck

-- import Emulator
initialState :: EmuState
initialState = EmuState {fileName = "", memory = "", pc = 0}

spec :: Spec
spec = do
  describe "runCPU" $ do
    describe "00E0" $ do
      let (resultState, resultBuffer) = runCPU "00E0" (initialState, (U.replicate 10 1))
      it "should clear the display" $ do
        resultBuffer `shouldBe` (U.replicate 10 0)
      it "should advance the pc by 2" $ do
        (pc resultState) `shouldBe` (pc initialState) + 2
    describe "00EE" $ do
      let 
      it "should return from subroutine" $ do
        runCPU "00EE" (initialState, (U.replicate 10 1)) `shouldBe` (initialState, (U.replicate 10 0))