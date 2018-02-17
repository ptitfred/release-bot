module Main where

import           Lib

import           GHC.IO.Handle    (BufferMode (NoBuffering), hSetBuffering)
import           GHC.IO.Handle.FD (stdout)

main :: IO ()
main = noBuffering >> service

noBuffering :: IO ()
noBuffering = hSetBuffering stdout NoBuffering
