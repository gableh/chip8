{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Graphics
import Emulator

main :: IO ()
main = do
    argv <- getArgs
    let filename = if null argv
                    then "roms/games/Space Invaders [David Winter].ch8"
                    else head argv
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow {
        windowInitialSize = V2 screenWidth screenHeight
    }
    renderer <- createRenderer window (-1) defaultRenderer

    rom::B.ByteString <- B.readFile filename

    startEmulator renderer rom
