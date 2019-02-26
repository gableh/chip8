{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import qualified Data.ByteString.Lazy as B

import Emulator

main :: IO ()
main = do
    let filename = "./roms/games/Bowling [Gooitzen van der Wal].ch8"
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow {
        windowInitialSize = V2 640 320
    }
    -- renderer <- createRenderer window (-1) defaultRenderer

    rom::B.ByteString <- B.readFile filename

    startEmulator window rom