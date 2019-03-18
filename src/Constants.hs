{-# LANGUAGE ScopedTypeVariables #-}
module Constants where

import           Data.Word
import SDL
import Data.Map.Strict as Map

type Word8Op = (Word8 -> Word8 -> Word8)

hexcode0 = [0xF0, 0x90, 0x90, 0x90, 0xF0]
hexcode1 = [0x20, 0x60, 0x20, 0x20, 0x70]
hexcode2 = [0xF0, 0x10, 0xF0, 0x80, 0xF0]
hexcode3 = [0xF0, 0x10, 0xF0, 0x10, 0xF0]
hexcode4 = [0x90, 0x90, 0xF0, 0x10, 0x10]
hexcode5 = [0xF0, 0x80, 0xF0, 0x10, 0xF0]
hexcode6 = [0xF0, 0x80, 0xF0, 0x90, 0xF0]
hexcode7 = [0xF0, 0x10, 0x20, 0x40, 0x40]
hexcode8 = [0xF0, 0x90, 0xF0, 0x90, 0xF0]
hexcode9 = [0xF0, 0x90, 0xF0, 0x10, 0xF0]
hexcodeA = [0xF0, 0x90, 0xF0, 0x90, 0x90]
hexcodeB = [0xE0, 0x90, 0xE0, 0x90, 0xE0]
hexcodeC = [0xF0, 0x80, 0x80, 0x80, 0xF0]
hexcodeD = [0xE0, 0x90, 0x90, 0x90, 0xE0]
hexcodeE = [0xF0, 0x80, 0xF0, 0x80, 0xF0]
hexcodeF = [0xF0, 0x80, 0xF0, 0x80, 0x80]
hexcodes::[Word8] = Prelude.map fromIntegral $ concat [
  hexcode0,hexcode1,hexcode2,hexcode3,
  hexcode4,hexcode5,hexcode6,hexcode7,
  hexcode8,hexcode9,hexcodeA,hexcodeB,
  hexcodeC,hexcodeD,hexcodeE,hexcodeF]

keycodeMap = Map.fromList [(Keycode0, 0), (Keycode1, 1),(Keycode2, 2),(Keycode3, 3),
              (Keycode4, 4),(Keycode5, 5),(Keycode6, 6),(Keycode7, 7),
              (Keycode8, 8),(Keycode9, 9),(KeycodeA, 10),(KeycodeB, 11),
              (KeycodeC, 12),(KeycodeD, 13),(KeycodeE, 14),(KeycodeF, 15)]
