{-# LANGUAGE ScopedTypeVariables #-}

module Instructions where

import           Constants
import           Control.Monad               (foldM)
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.ST
import Control.Lens
import           Data.Bits
import qualified Data.ByteString.Lazy        as B
import           Data.Int
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import           Data.Word
import           EmuState
import           Graphics
import           Utils                       (fromHex, getGenericNfromMem,
                                              toBits, updateMemAt)
loadVxSpriteIntoI :: Char -> GameState -> ST s GameState
loadVxSpriteIntoI xH (currentState, buffer) = do
  let x = fromHex [xH]
  let nextI = x * 5
  let nextState = currentState {pc = pc currentState + 2, i = nextI}
  return (nextState, buffer)

storeVx3IntoMemoryI :: Char -> GameState -> ST s GameState
storeVx3IntoMemoryI xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  let vx = (U.!) currentRegister x
  let currentI = i currentState
  let currentMemory = memory currentState
  let nextMemory = updateMemAt currentI (vx `div` 100)  (B.toStrict currentMemory)
  let nextMemory1 = updateMemAt (currentI+1) ((vx `div` 10) `mod` 10) nextMemory
  let finalMemory = updateMemAt (currentI+2) ((vx `mod` 100) `mod` 10) nextMemory1
  let nextState = currentState {pc = pc currentState + 2, memory = B.fromStrict finalMemory}
  return (nextState, buffer)

storeVxNIntoMemoryI :: Char -> GameState -> ST s GameState
storeVxNIntoMemoryI nH (currentState, buffer) = do
  let n = fromHex [nH]
  let currentRegister = register currentState
  let vN = U.take n currentRegister
  let currentI = i currentState
  let currentMemory = B.toStrict $ memory currentState
  let nextMemory = U.ifoldl (\memory index value -> updateMemAt (currentI + (fromIntegral index)::Word16) value memory) currentMemory vN
  let nextState = currentState {pc = pc currentState + 2, memory = B.fromStrict nextMemory}
  return (nextState, buffer)


loadMemoryIIntoVxN:: Char -> GameState -> ST s GameState
loadMemoryIIntoVxN nH (currentState, buffer) = do
  let n::Int64 = fromIntegral $ fromHex [nH]
  let currentRegister = register currentState
  let currentI::Int64 = fromIntegral $ i currentState
  let currentMemory = memory currentState
  let updateBytes = B.unpack $ getGenericNfromMem currentI (currentI + n) currentMemory
  let updateVector = U.zip (U.fromList [0..length updateBytes]) (U.fromList updateBytes)
  let nextRegister = U.update currentRegister updateVector
  let nextState = currentState {pc = pc currentState + 2, register = nextRegister}
  return (nextState, buffer)

addVxToI :: Char -> GameState -> ST s GameState
addVxToI xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  let addI::Word16 = fromIntegral $ (U.!) currentRegister x
  let nextState = currentState {pc = pc currentState + 2, i = i currentState + addI}
  return (nextState, buffer)

loadSoundVxTimer :: Char -> GameState -> ST s GameState
loadSoundVxTimer xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  let nextSoundTimer = (U.!) currentRegister x
  let nextState = currentState {pc = pc currentState + 2, soundTimer = nextSoundTimer}
  return (nextState, buffer)

loadVxKeyboard :: Char -> GameState -> ST s GameState
loadVxKeyboard xH (currentState, buffer) =
  case keycodes currentState of
    [] -> return (currentState, buffer)
    keycode:_ -> do
      let x = fromHex [xH]
      let currentRegister = register currentState
      registerM <- U.thaw currentRegister
      M.write registerM x keycode
      nextRegister <- U.freeze registerM
      let nextState = currentState {pc = pc currentState + 2, register = nextRegister}
      return (nextState, buffer)

loadDelayVxTimer :: Char -> GameState -> ST s GameState
loadDelayVxTimer xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  let nextDelayTimer = (U.!) currentRegister x
  let nextState = currentState {pc = pc currentState + 2, delayTimer = nextDelayTimer}
  return (nextState, buffer)

loadVxDelayTimer :: Char -> GameState -> ST s GameState
loadVxDelayTimer xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  registerM <- U.thaw currentRegister
  M.write registerM x (delayTimer currentState)
  nextRegister <- U.freeze registerM
  let nextState = currentState {pc = pc currentState + 2, register = nextRegister}
  return (nextState, buffer)

skipNextInstructionIfKeyPressed :: Char -> GameState -> ST s GameState
skipNextInstructionIfKeyPressed xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  let currentKeycodes = keycodes currentState

  let vxKeyPressed = getVxKeyPressed x currentRegister currentKeycodes
  let nextState = if vxKeyPressed
                    then currentState {pc = pc currentState + 4}
                    else currentState {pc = pc currentState + 2}
  return (nextState, buffer)

skipNextInstructionIfKeyNotPressed :: Char -> GameState -> ST s GameState
skipNextInstructionIfKeyNotPressed xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  let currentKeycodes = keycodes currentState

  let vxKeyPressed = getVxKeyPressed x currentRegister currentKeycodes
  let nextState = if vxKeyPressed
                    then currentState {pc = pc currentState + 2}
                    else currentState {pc = pc currentState + 4}
  return (nextState, buffer)

getVxKeyPressed :: Int -> U.Vector Word8 -> [Word8] -> Bool
getVxKeyPressed x currentRegister currentKeycodes = do
  let vx = (U.!) currentRegister x
  let vxKeyPressed = vx `elem` currentKeycodes
  vxKeyPressed

drawBuffer :: Char -> Char -> Char -> GameState -> ST s GameState
drawBuffer xH yH nH (currentState, buffer) = do
  let x = fromHex [xH]
  let y = fromHex [yH]
  let n = fromHex [nH]
  let currentRegister = register currentState
  let vx :: Int = fromIntegral $ (U.!) currentRegister x
  let vy :: Int = fromIntegral $ (U.!) currentRegister y
  let startI = fromIntegral (i currentState) :: Int64
  let startN = fromIntegral n :: Int64
  let bytes = B.unpack $ getGenericNfromMem startI (startI + startN - 1) (memory currentState)
  let bitArray = zip (map toBits bytes) [0 .. n]
  (nextBuffer, vF) <- xorBuffer buffer bitArray vx vy
  let currentRegister = register currentState
  registerM <- U.thaw currentRegister
  M.write registerM 15 vF
  nextRegister <- U.freeze registerM
  let nextState = currentState {pc = pc currentState + 2, register = nextRegister}
  return (nextState, nextBuffer)

xorBuffer :: PrimMonad m => U.Vector Word8 -> [([Word8], Int)] -> Int -> Int -> m (U.Vector Word8, Word8)
xorBuffer buffer bitArray x y = do
  let nextUpdate =
        foldl
          (\initial (bits, index) ->
             initial ++ zip [(intChipWidth * (y + index) + x) .. (intChipWidth * (y + index) + x + length bits)] bits)
          []
          bitArray
  let nextUpdateV = U.generate (length nextUpdate) (\x -> nextUpdate !! x)
  bufferM <- U.thaw buffer
  vF <-
    U.foldM
      (\initial (index, x) -> do
         let bufferX = (U.!) buffer index
         let newX = xor bufferX x
         M.write bufferM index newX
         if newX == 0 && bufferX == 1
           then return (1 :: Word8)
           else return initial)
      (0 :: Word8)
      nextUpdateV
  nextBuffer <- U.freeze bufferM
  return (nextBuffer, vF)

-- TODO figure out how to implement random
setRandomVx :: Char -> String -> GameState -> ST s GameState
setRandomVx xH kkH (currentState, buffer) = do
  let x = fromHex [xH]
  let kk = fromHex kkH
  let n = 255
  let resultX = kk .&. n
  let currentRegister = register currentState
  registerM <- U.thaw currentRegister
  M.write registerM x resultX
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState, buffer)

setRegisterI :: String -> GameState -> ST s GameState
setRegisterI byteH (currentState, buffer) = do
  let byte = fromHex byteH
  let nextState = currentState {i = byte, pc = pc currentState + 2}
  return (nextState, buffer)

subtractNRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
subtractNRegisterWithRegister = opRegisterWithRegister (flip (-)) (flip getBorrowFlag)

shlRegister :: Char -> GameState -> ST s GameState
shlRegister xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  let vx = (U.!) currentRegister x
  let resultX = shiftL vx 1
  let flag = shlFlagOp vx
  registerM <- U.thaw currentRegister
  M.write registerM 15 flag
  M.write registerM x resultX
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState, buffer)

shlFlagOp :: Word8 -> Word8
shlFlagOp vx = shiftR vx 7

shrRegister :: Char -> GameState -> ST s GameState
shrRegister xH (currentState, buffer) = do
  let x = fromHex [xH]
  let currentRegister = register currentState
  let vx = (U.!) currentRegister x
  let resultX = shiftR vx 1
  let flag = shrFlagOp vx
  registerM <- U.thaw currentRegister
  M.write registerM 15 flag
  M.write registerM x resultX
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState, buffer)

shrFlagOp :: Word8 -> Word8
shrFlagOp vx = vx .&. 1

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
  return (nextState, buffer)

subtractRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
subtractRegisterWithRegister = opRegisterWithRegister (-) getBorrowFlag

getBorrowFlag :: (Ord a, Num p) => a -> a -> p
getBorrowFlag vx vy =
  if vx > vy
    then 1
    else 0

addRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
addRegisterWithRegister = opRegisterWithRegister (+) getCarryFlag

getCarryFlag :: (Integral a, Num p) => a -> a -> p
getCarryFlag vx vy = do
  let x = fromIntegral vx
  let y = fromIntegral vy
  if x + y > 255
    then 1
    else 0

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
  return (nextState, buffer)

setRegisterWithRegister :: Char -> Char -> GameState -> ST s GameState
setRegisterWithRegister xH yH (currentState, buffer) = do
  let x = fromHex [xH]
  let y = fromHex [yH]
  let currentRegister = register currentState
  registerM <- U.thaw currentRegister
  M.write registerM x ((U.!) currentRegister y)
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState, buffer)

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
  return (nextState, buffer)

setRegisterWithByte :: Char -> String -> GameState -> ST s GameState
setRegisterWithByte xH byteH (currentState, buffer) = do
  let x = fromHex [xH]
  let byte = fromHex byteH
  let currentRegister = register currentState
  registerM <- U.thaw currentRegister
  M.write registerM x byte
  nextRegister <- U.freeze registerM
  let nextState = currentState {register = nextRegister, pc = pc currentState + 2}
  return (nextState, buffer)

skipNextInstructionIfRegistersOp :: (Word8 -> Word8 -> Bool) -> Char -> Char -> GameState -> ST s GameState
skipNextInstructionIfRegistersOp op xH yH (currentState, buffer) = do
  let y = fromHex [yH]
  let x = fromHex [xH]
  let currentRegister = register currentState
  if (U.!) currentRegister x `op` (U.!) currentRegister y
    then return (currentState {pc = pc currentState + 4}, buffer)
    else return (currentState {pc = pc currentState + 2}, buffer)

skipNextInstructionIfRegistersEqual :: Char -> Char -> GameState -> ST s GameState
skipNextInstructionIfRegistersEqual = skipNextInstructionIfRegistersOp (==)

skipNextInstructionIfRegistersNotEqual :: Char -> Char -> GameState -> ST s GameState
skipNextInstructionIfRegistersNotEqual = skipNextInstructionIfRegistersOp (/=)

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
  let nextPc = fromHex addrH
  let nextState = currentState {pc = nextPc, sp = nextSp, stack = nextStack}
  return (nextState, buffer)

jumpToAddr :: String -> GameState -> ST s GameState
jumpToAddr addrH (currentState, buffer) = do
  let addrI64 :: Int64 = fromHex addrH
  let nextState = currentState {pc = addrI64}
  return (nextState, buffer)

jumpWithV0 :: String -> GameState -> ST s GameState
jumpWithV0 addrH (currentState, buffer) = do
  let addrI64 :: Int64 = fromHex addrH
  let currentRegister = register currentState
  let nextPc = (fromIntegral $ (U.!) currentRegister 0) + addrI64
  let nextState = currentState {pc = nextPc}
  return (nextState, buffer)

returnFromSubRoutine :: GameState -> ST s GameState
returnFromSubRoutine (currentState, buffer) = do
  let currentStack = stack currentState
  let currentSp = sp currentState
  let nextState = currentState {pc = (U.!) currentStack currentSp + 2, sp = currentSp - 1}
  return (nextState, buffer)

clearDisplay :: GameState -> ST s GameState
clearDisplay (currentState, buffer) =
  return (currentState {pc = pc currentState + 2}, U.replicate (U.length buffer) 0 :: U.Vector Word8)
  -- bufferM <- U.thaw buffer
  -- U.unsafeFreeze buffer
