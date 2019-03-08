{-# LANGUAGE ScopedTypeVariables #-}

module Graphics where

import qualified Data.Vector.Storable.Mutable as M (IOVector)
import Data.Word (Word8)
import Foreign.C.Types (CInt)

chipHeight::CInt = 32
chipWidth::CInt = 64

screenHeight::CInt = chipHeight * 10
screenWidth::CInt = chipWidth * 10

bufferSize::CInt = chipHeight * chipWidth
