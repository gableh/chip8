{-# LANGUAGE OverloadedStrings #-}

module EmuState where

import qualified Data.ByteString.Lazy as B

import Constants
import Data.Int
import Data.Word (Word8, Word16)
import qualified Data.Vector.Unboxed as U
--import Data.Word (Word16, Word8)
--import Control.Monad.Trans.State

data EmuState = EmuState {
    fileName::String,
    memory::B.ByteString,
    pc::Int64,

    sp::Int,
    stack::U.Vector Int64,
    register::U.Vector Word8,
    i::Word16
} deriving (Show, Eq)

mkState :: String -> B.ByteString -> EmuState
mkState filename rom = EmuState filename (mkMemory rom) 512 0 (U.replicate 12 0) (U.replicate 16 0) 0

mkMemory :: B.ByteString -> B.ByteString
mkMemory rom = B.append (B.pack hexcodes) $ B.append (B.replicate 432 0) rom
