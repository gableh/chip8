{-# LANGUAGE OverloadedStrings #-}

module EmulatorSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed         as U
import Emulator
import EmuState
-- import Test.QuickCheck

-- import Emulator
initialState :: EmuState
initialState = EmuState {fileName = "", memory = "", pc = 0, sp = 2, stack = [3,2,400,4,5,6,7,8,9,10,22,13]}

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
      let (resultState, _) = runCPU "00EE" (initialState, (U.replicate 10 1))
      it "should set the program counter to the address at the top of the stack + 2" $ do
        pc resultState `shouldBe` 402
      it "should decrease stack pointer by 1" $ do
        sp resultState `shouldBe` 1
    describe "1nnn" $ do
      let (resultState, _) = runCPU "1ABC" (initialState, (U.replicate 10 1))
      it "should set the program counter to 0xABC + 2" $ do
        pc resultState `shouldBe` 2748 + 2 
