{-# LANGUAGE OverloadedStrings #-}

module EmulatorSpec
  ( spec
  ) where

import qualified Data.Vector.Unboxed as U
import           Emulator
import           EmuState
import           Test.Hspec

-- import Test.QuickCheck
-- import Emulator
initialState :: EmuState
initialState =
  EmuState
    { fileName = ""
    , memory = ""
    , pc = 0
    , sp = 2
    , stack = U.generate 12 (\x -> [3, 2, 400, 4, 5, 6, 7, 8, 9, 10, 22, 13] !! x)
    , register = U.replicate 16 0
    }

spec :: Spec
spec =
  describe "runCPU" $ do
    describe "00E0 - Clear the display" $ do
      let (resultState, resultBuffer) = runCPU "00E0" (initialState, U.replicate 10 1)
      it "should clear the display" $ resultBuffer `shouldBe` U.replicate 10 0
      it "should advance the pc by 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "00EE - Return from subroutine" $ do
      let (resultState, _) = runCPU "00EE" (initialState, U.replicate 10 1)
      it "should set the program counter to the address at the top of the stack + 2" $ pc resultState `shouldBe` 402
      it "should decrease stack pointer by 1" $ sp resultState `shouldBe` 1
    describe "1nnn - Jump to location nnn" $ do
      let (resultState, _) = runCPU "1ABC" (initialState, U.replicate 10 1)
      it "should set the program counter to 0xABC + 2" $ pc resultState `shouldBe` 2748 + 2
    describe "2nnn - Call subroutine at nnn" $ do
      let (resultState, _) = runCPU "2ABC" (initialState, U.replicate 10 1)
      it "should increment the stack pointer" $ sp resultState `shouldBe` sp initialState + 1
      it "should set the current pc to the top of the stack" $
        (U.!) (stack resultState) (sp resultState) `shouldBe` pc initialState
      it "should set the current pc to be NNN + 2" $ pc resultState `shouldBe` 2748 + 2
    describe "3xkk - Skip next instruction if Vx = kk" $ do
      describe "Vx == kk" $ do
        let newRegister = U.replicate 16 255
        let (resultState, _) = runCPU "3AFF" (initialState {register = newRegister}, U.replicate 10 1)
        it "should set the next pc to be current pc + 4" $ pc resultState `shouldBe` pc initialState + 4
      describe "Vx != kk" $ do
        let newRegister = U.replicate 16 0
        let (resultState, _) = runCPU "3AFF" (initialState {register = newRegister}, U.replicate 10 1)
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "4xkk - Skip next instruction if Vx != kk" $ do
      describe "Vx == kk" $ do
        let newRegister = U.replicate 16 255
        let (resultState, _) = runCPU "4AFF" (initialState {register = newRegister}, U.replicate 10 1)
        it "should set the next pc to be current pc + 4" $ pc resultState `shouldBe` pc initialState + 2
      describe "Vx != kk" $ do
        let newRegister = U.replicate 16 0
        let (resultState, _) = runCPU "4AFF" (initialState {register = newRegister}, U.replicate 10 1)
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 4
    describe "5xy0 - Skip next instruction if Vx = Vy" $ do
      describe "Vx == Vy" $ do
        let newRegister = U.replicate 16 255
        let (resultState, _) = runCPU "5AF0" (initialState {register = newRegister}, U.replicate 10 1)
        it "should set the next pc to be current pc + 4" $ pc resultState `shouldBe` pc initialState + 4
      describe "Vx != Vy" $ do
        let newRegister = U.generate 16 (\x -> [1 .. 16] !! x)
        let (resultState, _) = runCPU "5AF0" (initialState {register = newRegister}, U.replicate 10 1)
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "6xkk - LD Vx, byte" $ do
      let newRegister = U.replicate 16 0
      let (resultState, _) = runCPU "6AFF" (initialState {register = newRegister}, U.replicate 10 1)
      it "should set register Vx to kk" $ do
        let resultRegister = register resultState
        (U.!) resultRegister 10 `shouldBe` 255
      it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "7xkk - ADD Vx, byte" $ do
      let newRegister = U.replicate 16 10
      let (resultState, _) = runCPU "7AAA" (initialState {register = newRegister}, U.replicate 10 1)
      it "should set register Vx to register Vx + kk" $ do
        let resultRegister = register resultState
        (U.!) resultRegister 10 `shouldBe` 180
      it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "8xy0 - LD Vx, Vy" $ do
      let newRegister = U.generate 16 (\x -> [1 .. 16] !! x)
      let (resultState, _) = runCPU "80F0" (initialState {register = newRegister}, U.replicate 10 1)
      it "should set register V0 to 16" $ do
        let resultRegister = register resultState
        (U.!) resultRegister 0 `shouldBe` 16
      it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "8xy1 - OR Vx, Vy" $ do
      let newRegister = U.generate 16 (\x -> [0 .. 15] !! x)
      let (resultState, _) = runCPU "8691" (initialState {register = newRegister}, U.replicate 10 1)
      it "should perform an OR operation on V6 and V9 and store the result in V6" $ do
        let resultRegister = register resultState
        (U.!) resultRegister 6 `shouldBe` 15
      it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "8xy2 - AND Vx, Vy" $ do
      let newRegister = U.generate 16 (\x -> [0 .. 15] !! x)
      let (resultState, _) = runCPU "8692" (initialState {register = newRegister}, U.replicate 10 1)
      it "should perform an AND operation on V6 and V9 and store the result in V6" $ do
        let resultRegister = register resultState
        (U.!) resultRegister 6 `shouldBe` 0
      it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "8xy3 - XOR Vx, Vy" $ do
      let newRegister = U.generate 16 (\x -> [0 .. 15] !! x)
      let (resultState, _) = runCPU "8663" (initialState {register = newRegister}, U.replicate 10 1)
      it "should perform a XOR operation on V6 and V6 and store the result in V6" $ do
        let resultRegister = register resultState
        (U.!) resultRegister 6 `shouldBe` 0
      it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "8xy4 - ADD Vx, Vy" $ do
      describe "if Vx  + Vy < 255" $ do
        let newRegister = U.generate 16 (\x -> ([0..14] ++ [0]) !! x)
        let (resultState, _) = runCPU "8674" (initialState {register = newRegister}, U.replicate 10 1)
        it "should perform an ADD operation on V6 and V6 and store the result in V6" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 6 `shouldBe` 13
        it "should not set V15 to 1" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 15 `shouldBe` 0
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2

      describe "if Vx  + Vy > 255" $ do
        let newRegister = (U.snoc) (U.replicate 15 255) 0
        let (resultState, _) = runCPU "8674" (initialState {register = newRegister}, U.replicate 10 1)
        it "should perform an ADD operation on V6 and V6 and store the result in V6" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 6 `shouldBe` 254
        it "should set V15 to 1" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 15 `shouldBe` 1
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2
    describe "8xy5 - SUB Vx, Vy" $ do
      describe "if Vx > Vy" $ do
        let newRegister = U.generate 16 (\x -> ([0..14] ++ [0]) !! x)
        let (resultState, _) = runCPU "8765" (initialState {register = newRegister}, U.replicate 10 1)
        it "should perform a SUB operation on V7 and V6 and store the result in V6" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 7 `shouldBe` 1
        it "should not set V15 to 1" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 15 `shouldBe` 1
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2

      describe "if Vx < Vy" $ do
        let newRegister = U.generate 16 (\x -> ([0..14] ++ [0]) !! x)
        let (resultState, _) = runCPU "8675" (initialState {register = newRegister}, U.replicate 10 1)
        it "should perform a SUB operation on V6 and V7 and store the result in V6" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 6 `shouldBe` 255
        it "should set V15 to 1" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 15 `shouldBe` 0
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2

      -- https://www.reddit.com/r/EmuDev/comments/72dunw/chip8_8xy6_help/
      describe "8xy6 - SHR Vx {, Vy}" $ do
        it "should set VF to 1 if LSB of VF is 1" $ do
          let newRegister = U.generate 16 (\x -> ([0..14] ++ [0]) !! x)
          let (resultState, _) = runCPU "8776" (initialState {register = newRegister}, U.replicate 10 1)
          let resultRegister = register resultState
          (U.!) resultRegister 15 `shouldBe` 1

        it "should set VF to 0 if LSB of VF is 0" $ do
          let newRegister = U.generate 16 (\x -> ([0..14] ++ [0]) !! x)
          let (resultState, _) = runCPU "8676" (initialState {register = newRegister}, U.replicate 10 1)
          let resultRegister = register resultState
          (U.!) resultRegister 15 `shouldBe` 0


    describe "8xy7 - SUBN Vx, Vy" $ do
      describe "if Vy > Vx" $ do
        let newRegister = U.generate 16 (\x -> ([0..14] ++ [0]) !! x)
        let (resultState, _) = runCPU "8677" (initialState {register = newRegister}, U.replicate 10 1)
        it "should perform a SUBN operation on V7 and V6 and store the result in V6" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 6 `shouldBe` 1
        it "should not set V15 to 1" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 15 `shouldBe` 1
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2

      describe "if Vy < Vx" $ do
        let newRegister = U.generate 16 (\x -> ([0..14] ++ [0]) !! x)
        let (resultState, _) = runCPU "8767" (initialState {register = newRegister}, U.replicate 10 1)
        it "should perform a SUB operation on V6 and V7 and store the result in V6" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 7 `shouldBe` 255
        it "should set V15 to 1" $ do
          let resultRegister = register resultState
          (U.!) resultRegister 15 `shouldBe` 0
        it "should set the next pc to be current pc + 2" $ pc resultState `shouldBe` pc initialState + 2

