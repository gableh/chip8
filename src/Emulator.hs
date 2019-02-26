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
import EmuState
-- import Graphics

-- type GameState = (EmuState, Vector Word8)

startEmulator :: Monad m => Window -> B.ByteString -> m ()
startEmulator window rom = do
  -- let initialState = mkState
  return ()

runCPU :: String -> U.Vector Word8 -> U.Vector Word8
runCPU opcode buffer = runST $ do 
  case opcode of
    "00E0" -> return (U.replicate (U.length buffer) 0 :: U.Vector Word8)

  -- bufferM <- U.thaw buffer

  -- U.unsafeFreeze bufferM
