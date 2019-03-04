{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Emulator where

import SDL (Window)
import Control.Monad.ST

-- import Control.Monad.Primitive    (PrimMonad, PrimState)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import Data.Word (Word8)
import Data.Int
import Data.Bits
import EmuState
-- import Graphics
import Utils (fromHex)

type GameState = (EmuState, U.Vector Word8)
type Word8Op = (Word8 -> Word8 -> Word8)
startEmulator :: Monad m => Window -> B.ByteString -> m ()
startEmulator window rom = return ()

runCPU :: String -> GameState -> GameState
runCPU opcode gameState@(currentState, buffer) =
  runST $
  case opcode of
    "00E0" -> clearDisplay gameState
    "00EE" -> returnFromSubRoutine gameState
    '1':xs -> jumpToAddr xs gameState
    '2':xs -> callSubroutine xs gameState
    '3':(x:byteH) -> skipNextInstructionIfEqual x byteH gameState
    '4':(x:byteH) -> skipNextInstructionIfNotEqual x byteH gameState
    '5':x:y:['0'] -> skipNextInstructionIfRegistersEqual x y gameState
    '6':(x:byteH) -> setRegisterWithByte x byteH gameState
    '7':(x:byteH) -> addRegister x byteH gameState
    '8':x:y:['0'] -> setRegisterWithRegister x y gameState
    '8':x:y:['1'] -> orRegisterWithRegister x y gameState
    '8':x:y:['2'] -> andRegisterWithRegister x y gameState
    '8':x:y:['3'] -> xorRegisterWithRegister x y gameState
    '8':x:y:['4'] -> addRegisterWithRegister x y gameState
    '8':x:y:['5'] -> subtractRegisterWithRegister x y gameState

opRegisterWithRegister :: Word8Op -> Word8Op -> Char -> Char -> GameState -> ST s GameState
opRegisterWithRegister op flagOp xH yH (currentState, buffer) = do
  let x = fromHex [xH]
  let y = fromHex [yH]
  let currentRegister = register currentState
  let vx = (U.!) currentRegister x
  let vy = (U.!) currentRegister y
  let resultX = op vx vy
  let flag = flagOp vx vy
  registerM <- U.thaw currentRegister
  M.write registerM x resultX
  M.write registerM 15 flag
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState ,buffer)

subtractRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
subtractRegisterWithRegister = opRegisterWithRegister (-) getBorrowFlag

getBorrowFlag :: (Ord a, Num p) => a -> a -> p
getBorrowFlag vx vy = if vx > vy then 1 else 0

addRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
addRegisterWithRegister = opRegisterWithRegister (+) getCarryFlag

getCarryFlag :: (Integral a, Num p) => a -> a -> p
getCarryFlag vx vy = do
  let x = fromIntegral vx
  let y = fromIntegral vy
  if x + y > 255 then 1 else 0

xorRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
xorRegisterWithRegister = bitwiseRegisterWithRegister xor

andRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
andRegisterWithRegister = bitwiseRegisterWithRegister (.&.)

orRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
orRegisterWithRegister = bitwiseRegisterWithRegister (.|.)


bitwiseRegisterWithRegister :: Word8Op -> Char -> Char -> GameState -> ST s GameState
bitwiseRegisterWithRegister bitOp xH yH (currentState, buffer) = do
  let x = fromHex [xH]
  let y = fromHex [yH]
  let currentRegister = register currentState
  let vx = (U.!) currentRegister x
  let vy = (U.!) currentRegister y
  let resultX = bitOp vx vy
  registerM <- U.thaw currentRegister
  M.write registerM x resultX
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState ,buffer)

setRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
setRegisterWithRegister xH yH (currentState, buffer) = do
  let x = fromHex [xH]
  let y = fromHex [yH]
  let currentRegister = register currentState
  registerM <- U.thaw currentRegister
  M.write registerM x ((U.!) currentRegister y)
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState ,buffer)

addRegister :: Char -> String -> GameState -> ST s GameState
addRegister xH byteH (currentState, buffer) = do
  let x = fromHex [xH]
  let byte = fromHex byteH
  let currentRegister = register currentState
  let currentRegisterXValue = (U.!) currentRegister x
  registerM <- U.thaw currentRegister
  M.write registerM x (byte + currentRegisterXValue)
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState ,buffer)

setRegisterWithByte :: Char -> String -> GameState -> ST s GameState
setRegisterWithByte xH byteH (currentState, buffer) = do
  let x = fromHex [xH]
  let byte = fromHex byteH
  let currentRegister = register currentState
  registerM <- U.thaw currentRegister
  M.write registerM x byte
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState ,buffer)


skipNextInstructionIfRegistersEqual :: Char -> Char -> GameState -> ST s GameState
skipNextInstructionIfRegistersEqual xH yH (currentState, buffer) = do
  let y = fromHex [yH]
  let x = fromHex [xH]
  let currentRegister = register currentState
  if (U.!) currentRegister x == (U.!) currentRegister y
    then return (currentState {pc = pc currentState + 4}, buffer)
    else return (currentState {pc = pc currentState + 2}, buffer)

skipNextInstructionIfNotEqual :: Char -> String -> GameState -> ST s GameState
skipNextInstructionIfNotEqual xH byteH (currentState, buffer) = do
  let byte = fromHex byteH
  let x = fromHex [xH]
  let currentRegister = register currentState
  if (U.!) currentRegister x == byte
    then return (currentState {pc = pc currentState + 2}, buffer)
    else return (currentState {pc = pc currentState + 4}, buffer)

skipNextInstructionIfEqual :: Char -> String -> GameState -> ST s GameState
skipNextInstructionIfEqual xH byteH (currentState, buffer) = do
  let byte = fromHex byteH
  let x = fromHex [xH]
  let currentRegister = register currentState
  if (U.!) currentRegister x == byte
    then return (currentState {pc = pc currentState + 4}, buffer)
    else return (currentState {pc = pc currentState + 2}, buffer)

callSubroutine :: String -> GameState -> ST s GameState
callSubroutine addrH (currentState, buffer) = do
  let nextSp = sp currentState + 1
  stackM <- U.thaw (stack currentState)
  M.write stackM nextSp (pc currentState)
  nextStack <- U.freeze stackM
  let nextPc = fromHex addrH + 2
  let nextState = currentState { pc = nextPc, sp = nextSp, stack = nextStack }
  return (nextState, buffer)


jumpToAddr :: String -> GameState -> ST s GameState
jumpToAddr addrH (currentState, buffer) = do
  let addrI64::Int64 = fromHex addrH
  let nextState = currentState { pc = addrI64 + 2 }
  return (nextState, buffer)

returnFromSubRoutine :: GameState -> ST s GameState
returnFromSubRoutine (currentState, buffer) = do
  let currentStack = stack currentState
  let currentSp = sp currentState
  let nextState = currentState { pc = (U.!) currentStack currentSp + 2, sp = currentSp - 1}
  return (nextState, buffer)

clearDisplay :: GameState -> ST s GameState
clearDisplay (currentState, buffer) =
  return (currentState {pc = pc currentState + 2}, U.replicate (U.length buffer) 0 :: U.Vector Word8)
  -- bufferM <- U.thaw buffer

  -- U.unsafeFreeze buffer
