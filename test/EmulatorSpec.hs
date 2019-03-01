{-# LANGUAGE OverloadedStrings #-}

module EmulatorSpec (spec) where

import Test.Hspec
import qualified Data.Vector.Unboxed         as U
import Emulator
import EmuState
-- import Test.QuickCheck

-- import Emulator
initialState :: EmuState
initialState = EmuState {fileName = "", memory = "", pc = 0, sp = 2, stack = U.generate 12 (\x-> [3,2,400,4,5,6,7,8,9,10,22,13]!!x) }

spec :: Spec
spec =
  describe "runCPU" $ do
    describe "00E0" $ do
      let (resultState, resultBuffer) = runCPU "00E0" (initialState, U.replicate 10 1)
      it "should clear the display" $ resultBuffer `shouldBe` U.replicate 10 0
      it "should advance the pc by 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "00EE" $ do
      let (resultState, _) = runCPU "00EE" (initialState, U.replicate 10 1)
      it "should set the program counter to the address at the top of the stack + 2" $ pc resultState `shouldBe` 402
      it "should decrease stack pointer by 1" $ sp resultState `shouldBe` 1
    describe "1nnn" $ do
      let (resultState, _) = runCPU "1ABC" (initialState, U.replicate 10 1)
      it "should set the program counter to 0xABC + 2" $ pc resultState `shouldBe` 2748 + 2
    describe "2nnn" $ do
      let (resultState, _) = runCPU "2ABC" (initialState, U.replicate 10 1)
      it "should increment the stack pointer" $ sp resultState `shouldBe` sp initialState + 1
      it "should set the current pc to the top of the stack" $
        (U.!) (stack resultState) (sp resultState) `shouldBe` pc initialState
      it "should set the current pc to be NNN + 2" $ pc resultState `shouldBe` 2748 + 2
    describe "3xkk" $ do
      describe "x == kk" $ do
        let newStack = U.replicate 12 255
        let (resultState, _) = runCPU "3AFF" (initialState {stack = newStack}, U.replicate 10 1)
        it "should set the next pc to be current pc + 4" $ pc resultState `shouldBe` pc initialState + 4
      describe "x != kk" $ do
        let newStack = U.replicate 12 0
        let (resultState, _) = runCPU "3AFF" (initialState {stack = newStack}, U.replicate 10 1)
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2

