module Main where

import WithRawInput
import IntegerCalculator
import Calculator
import ConvertibleCalculator

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

-- backspace takes a string and removes the last character
backspace :: String -> String
backspace [] = []
backspace acc =
    case reverse acc of
        [] -> []
        _:xs -> reverse xs

-- consumeChar adds the character c to the accumulator and attempts
-- to consume a token from the accumulator.
consumeChar :: Calculator a => a -> [Char] -> Char -> (a, [Char])
consumeChar calc acc c =
    let
        newacc = reverse (c:(reverse acc))
    in calcConsume calc newacc

-- processChar processes the next input character.
-- It returns the new state of the Calculator and any as-yet-unconsumed
-- input.
processChar :: Calculator a => a -> [Char] -> Char -> (a, [Char])
-- handle both DEL and BS as a backspace
processChar calc acc '\DEL' = (calc, backspace acc)
processChar calc acc '\BS' = (calc, backspace acc)
-- Space and newline don't have any inherent meaning except that they
-- force an end to any token that has been accumulating. They throw away
-- any accumulated characters so it's always possible to use one of these
-- characters to reset the input.
processChar calc acc '\n' = case consumeChar calc acc '\n' of (c, _) -> (c, "")
processChar calc acc ' ' = case consumeChar calc acc ' ' of (c, _) -> (c, "")
processChar calc acc c = consumeChar calc acc c

-- showCalculator updates the calculator state display.
-- It does this by erasing anything that was previously displayed and
-- redisplaying the entire state.
showCalculator :: Calculator a => a -> [Char] -> IO()
showCalculator calc acc = do
    -- ANSI escapes: clear from cursor to beginning of screen, move to position (1,1)
    putStr "\ESC[1J\ESC[H"
    calcDisplay calc
    putStr ("> " ++ acc)

-- doCalculator is the calculator main loop, consuming input characters one at a
-- time and displaying the updated state of the calculator at each step.
-- It also handles certain special operations that can't be expressed as EngineFn.
-- These include exiting the program, and switching to a different arithmetic mode.
doCalculator :: (Calculator a, ConvertibleCalculator a) => a -> [Char] -> [Char] -> IO ()
doCalculator _ _ [] = return ()
doCalculator initialCalc acc (x:xs) =
    let (newCalc, newAcc) = processChar initialCalc acc x
    in case newAcc of 
        "\\x" -> return ()
        "\\f" -> startCalculator (calcToFloat newCalc) xs
        "\\i" -> startCalculator (calcToInteger newCalc) xs
        "\\8" -> startCalculator (calcToWord8 newCalc) xs
        "\\16" -> startCalculator (calcToWord16 newCalc) xs
        "\\32" -> startCalculator (calcToWord32 newCalc) xs
        "\\64" -> startCalculator (calcToWord64 newCalc) xs
        _ -> do
            showCalculator newCalc newAcc
            doCalculator newCalc newAcc xs

-- startCalculator displays the initial state of the calculator and then enters
-- the main loop.
startCalculator :: (Calculator a, ConvertibleCalculator a) => a -> [Char] -> IO ()
startCalculator calc input = do
    showCalculator calc ""
    doCalculator calc "" input

-- main configures the terminal for unbuffered output and raw input, and then
-- starts the calculator.
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- getContents
    withRawInput 0 0 $ startCalculator defaultCalculator input
