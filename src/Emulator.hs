{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Emulator where

import           Control.Monad.ST
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import           SDL
import System.Random
import qualified Data.Char as C
import qualified Data.ByteString.Lazy        as B
import           EmuState
import           Utils                       (getOpcode)
import qualified Data.Vector.Unboxed as U
import Instructions
import Graphics
import Foreign.C.Types (CInt)

startEmulator :: MonadIO m => Renderer -> B.ByteString -> m ()
startEmulator renderer rom = do
  let state = mkState "filename" (mkMemory rom)
  runEmulator renderer (state, U.replicate (fromIntegral (chipHeight * chipWidth) :: Int) 1)

runEmulator :: MonadIO m => Renderer -> GameState -> m ()
runEmulator renderer gameState@(currentState, buffer) = do
  events <- pollEvents
  let eventIsQPress event = case eventPayload event of
          KeyboardEvent keyboardEvent ->
              keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  let opcode = getOpcode (pc currentState) (memory currentState)
  liftIO $ print opcode
  let nextGameState = runCPU opcode gameState
  unless qPressed (runEmulator renderer nextGameState)

runCPU :: String -> GameState -> GameState
runCPU opcode gameState@(currentState, buffer) =
  runST $
  case map C.toUpper opcode of
    "E0"        -> clearDisplay gameState
    "EE"        -> returnFromSubRoutine gameState
    '1':xs        -> jumpToAddr xs gameState
    '2':xs        -> callSubroutine xs gameState
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
    _ -> error "fsd"

