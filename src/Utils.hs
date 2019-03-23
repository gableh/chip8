module Utils where

import Numeric
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Bits
import Data.Word
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ByteString (ByteString)
import Data.Vector.Storable (modify)
import Data.Vector.Storable.ByteString
import Data.Vector.Storable.Mutable (write)

getTime :: IO Integer
getTime = round . (* 1000) <$> getPOSIXTime

fromHex :: (Eq a, Num a) => String -> a
fromHex n = fst (head (readHex n))

getInstruction :: Int64 -> B.ByteString -> B.ByteString
getInstruction n memory = B.snoc (B.snoc B.empty (B.index memory (n))) (B.index memory (n + 1))

getGenericNfromMem :: Int64 -> Int64 -> B.ByteString -> B.ByteString
getGenericNfromMem startN finishN memory =
  if finishN == startN
    then B.snoc B.empty (B.index memory startN)
    else B.snoc (getGenericNfromMem startN (finishN - 1) memory) (B.index memory finishN)

getOpcode pc memory = do
  let instruction = getInstruction pc memory
  showHex (runGet getWord16be instruction) ""

toBits x = reverse [if testBit x i then 1::Word8 else 0::Word8 | i <- [0.. finiteBitSize x - 1]]

updateMemAt :: Word16 -> Word8 -> ByteString -> ByteString
updateMemAt n x = vectorToByteString . modify inner . byteStringToVector
  where
    inner vector = write vector (fromIntegral n) x