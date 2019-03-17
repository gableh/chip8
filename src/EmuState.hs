{-# LANGUAGE OverloadedStrings #-}

module EmuState where

import qualified Data.ByteString.Lazy as B
import SDL
import Constants
import Data.Int
import Data.Word (Word8, Word16)
import qualified Data.Vector.Unboxed as U

type GameState = (EmuState, U.Vector Word8)
data EmuState = EmuState {
    fileName::String,
    memory::B.ByteString,
    pc::Int64,

    sp::Int,
    stack::U.Vector Int64,
    register::U.Vector Word8,
    i::Word16,
    keycodes::[Word8],
    delayTimer::Word8,
    soundTimer::Word8
} deriving (Show, Eq)

mkState :: String -> B.ByteString -> EmuState
mkState filename rom = EmuState filename rom 512 0 (U.replicate 12 0) (U.replicate 16 0) 0 [] 0 0

mkMemory :: B.ByteString -> B.ByteString
mkMemory rom = do
  let memory = B.append (B.pack hexcodes) $ B.append (B.replicate 432 0) rom
  B.append memory (B.replicate (4096 - B.length memory) 0)
