module EmuState where

import qualified Data.ByteString.Lazy as B
--import Data.Int
--import Data.Word (Word16, Word8)
--import Control.Monad.Trans.State

data EmuState = EmuState {
--    pc::Int64,
--    i::Word16,
    fileName::String,
    memory::B.ByteString
--    _register::[Word8],
--    sp::Int,,
--    _stack::[Int64]
} deriving (Show, Eq)
