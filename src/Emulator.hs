{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Emulator where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as C
import Data.Maybe
import Data.Int
import qualified Data.Map.Strict as M
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Data.Word
import Data.Time
import EmuState
import Foreign.C.Types (CInt)
import Graphics
import Instructions
import SDL
import Constants
import System.Random
import Utils (getOpcode, getTime)

startEmulator :: MonadIO m => Renderer -> B.ByteString -> m ()
startEmulator renderer rom = do
  currentTime <- liftIO getTime
  let state = mkState "filename" (mkMemory rom) currentTime
  runEmulator renderer (state, U.replicate (fromIntegral (chipHeight * chipWidth + 1) :: Int) 0)

setDelayTimer 0 = 0
setDelayTimer dt = dt - 1

runEmulator :: MonadIO m => Renderer -> GameState -> m ()
runEmulator renderer gameState@(currentState, buffer) = do
  currentTime <- liftIO getTime
  let (nextDelayTimer, nextTime) =
        if currentTime - systemTime currentState > 16
          then (setDelayTimer $ delayTimer currentState, currentTime)
          else (delayTimer currentState, systemTime currentState)
  let rendererColor = rendererDrawColor renderer
  rendererColor $= V4 0 0 0 0
  clear renderer
  rendererColor $= V4 255 255 255 0
  event <- pollEvent
  let currentKeycodes = keycodes currentState
  let keycodes = maybe currentKeycodes (updateKeycodes currentKeycodes) event
  let qPressed = maybe False eventIsQPress event
  let opcode = getOpcode (pc currentState) (memory currentState)
  let nextGameState@(nextState, nextBuffer) = runCPU opcode (currentState {
    keycodes,
    delayTimer = nextDelayTimer,
    systemTime = nextTime}, buffer)
  let pixels :: Vector Int = U.elemIndices 1 nextBuffer
  let rectangles = S.generate (U.length pixels) (getXYPixel . (U.!) pixels)
  fillRects renderer rectangles
  present renderer
  unless qPressed (runEmulator renderer nextGameState)

updateKeycodes :: [Word8] -> Event -> [Word8]
updateKeycodes currentKeycodes event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      case keyboardEventKeyMotion keyboardEvent of
        Pressed -> do
          let keycode = M.lookup (keysymKeycode (keyboardEventKeysym keyboardEvent)) keycodeMap
          maybe currentKeycodes (\keycode -> fromIntegral (keycode) : currentKeycodes) keycode
        Released -> do
          let keycode = M.lookup (keysymKeycode (keyboardEventKeysym keyboardEvent)) keycodeMap
          maybe currentKeycodes (\keycode -> filter ((fromIntegral $ keycode) /=) currentKeycodes) keycode
    _ -> currentKeycodes

eventIsQPress :: Event -> Bool
eventIsQPress event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    _ -> False

getXYPixel :: Int -> Rectangle CInt
getXYPixel pixel =
  let x :: CInt = (fromIntegral pixel `mod` chipWidth) * 10
   in let y :: CInt = (fromIntegral pixel `div` chipWidth) * 10
       in Rectangle (P $ V2 x y) (V2 (10 :: CInt) (10 :: CInt))

runCPU :: String -> GameState -> GameState
runCPU opcode gameState@(currentState, buffer) =
  runST $
  case map C.toUpper opcode of
    "E0" -> clearDisplay gameState
    "EE" -> returnFromSubRoutine gameState
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
    '8':x:_:['6'] -> shrRegister x gameState
    '8':x:y:['7'] -> subtractNRegisterWithRegister x y gameState
    '8':x:y:['E'] -> shlRegister x gameState
    '9':x:y:['0'] -> skipNextInstructionIfRegistersNotEqual x y gameState
    'A':byteH -> setRegisterI byteH gameState
    'B':byteH -> jumpWithV0 byteH gameState
    'C':x:kk -> setRandomVx x kk gameState
    'D':x:y:[n] -> drawBuffer x y n gameState
    'E':x:"9E" -> skipNextInstructionIfKeyPressed x gameState
    'E':x:"A1" -> skipNextInstructionIfKeyNotPressed x gameState
    'F':x:"07" -> loadVxDelayTimer x gameState
    'F':x:"15" -> loadDelayVxTimer x gameState
    'F':x:"0A" -> loadVxKeyboard x gameState
    'F':x:"18" -> loadSoundVxTimer x gameState
    'F':x:"1E" -> addVxToI x gameState
    'F':x:"29" -> loadVxSpriteIntoI x gameState
    'F':x:"33" -> storeVx3IntoMemoryI x gameState
    'F':x:"55" -> storeVxNIntoMemoryI x gameState
    'F':x:"65" -> loadMemoryIIntoVxN x gameState

