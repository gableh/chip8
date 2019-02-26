{-# LANGUAGE OverloadedStrings #-}

module EmuState where

import qualified Data.ByteString.Lazy as B

import Constants
import Data.Int
--import Data.Word (Word16, Word8)
--import Control.Monad.Trans.State

data EmuState = EmuState {
    --    i::Word16,
    fileName::String,
    memory::B.ByteString,
    pc::Int
    
    --    _register::[Word8],
--    sp::Int,,
--    _stack::[Int64]
} deriving (Show, Eq)

mkState :: String -> B.ByteString -> EmuState
mkState filename rom = EmuState filename (mkMemory rom) 512

mkMemory :: B.ByteString -> B.ByteString
mkMemory rom = B.append (B.pack hexcodes) $ B.append (B.replicate 432 0) rom