
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Emulator where

import SDL (Window)
import Control.Monad.Trans.State
import Control.Monad.ST

-- import Control.Monad.Primitive    (PrimMonad, PrimState)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import Data.Word (Word8)
import Data.Int
import EmuState
-- import Graphics
import Utils (fromHex)

type GameState = (EmuState, U.Vector Word8)

startEmulator :: Monad m => Window -> B.ByteString -> m ()
startEmulator window rom = do
  -- let initialState = mkState
  return ()

runCPU :: String -> GameState -> GameState
runCPU opcode gameState@(currentState, buffer) = runST $ do
  case opcode of
    "00E0" -> clearDisplay gameState
    "00EE" -> returnFromSubRoutine gameState
    '1':xs -> jumpToAddr xs gameState

jumpToAddr :: String -> GameState -> ST s GameState
jumpToAddr addrH (currentState, buffer) = do
  let addrI64::Int64 = fromHex addrH
  let nextState = currentState { pc = addrI64 + 2 }
  return (nextState, buffer)

returnFromSubRoutine :: GameState -> ST s GameState
returnFromSubRoutine (currentState, buffer) = do
  let currentStack = stack currentState
  let currentSp = sp currentState
  let nextState = currentState { pc = currentStack!!currentSp + 2, sp = currentSp - 1}
  return (nextState, buffer)

clearDisplay :: GameState -> ST s GameState
clearDisplay (currentState, buffer) = return (currentState {pc = pc currentState + 2}, (U.replicate (U.length buffer) 0 :: U.Vector Word8))
  -- bufferM <- U.thaw buffer

  -- U.unsafeFreeze bufferM
