{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Emulator where

import qualified Data.ByteString.Lazy as B

runEmulator :: String -> IO ()
runEmulator filename = do
 rom::B.ByteString <- B.readFile filename
 return ()
