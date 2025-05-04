module Main where

import Runner
import WithRawInput

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

-- main configures the terminal for unbuffered output and raw input, and then
-- starts the calculator.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- getContents
    withRawInput 0 0 $ startCalculator defaultCalculator input
