module Utils where

import Numeric
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Bits
import Data.Word

fromHex :: (Eq a, Num a) => String -> a
fromHex n = fst (head (readHex n))

getInstruction :: Int64 -> B.ByteString -> B.ByteString
getInstruction n memory = B.snoc (B.snoc B.empty (B.index memory (n))) (B.index memory (n + 1))

getOpcode pc memory = do
  let instruction = getInstruction pc memory
  showHex (runGet getWord16be instruction) ""

toBits x = reverse [if testBit x i then 1::Word8 else 0::Word8 | i <- [0.. finiteBitSize x - 1]]