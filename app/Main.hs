{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import qualified Data.ByteString.Lazy as B
import Graphics
import Emulator

main :: IO ()
main = do
    let filename = "roms/BC_test.ch8"
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow {
        windowInitialSize = V2 screenWidth screenHeight
    }
    renderer <- createRenderer window (-1) defaultRenderer

    rom::B.ByteString <- B.readFile filename

    startEmulator renderer rom
