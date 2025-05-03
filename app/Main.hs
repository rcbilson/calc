module Main where

import WithRawInput
import IntegerCalculator
import Calculator
import ConvertibleCalculator

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

backspace :: String -> String
backspace [] = []
backspace acc =
    case reverse acc of
        [] -> []
        _:xs -> reverse xs

consumeChar :: Calculator a => a -> [Char] -> Char -> (a, [Char])
consumeChar calc acc c =
    let
        newacc = reverse (c:(reverse acc))
    in calcConsume calc newacc

parseChar :: Calculator a => a -> [Char] -> Char -> (a, [Char])
parseChar calc acc '\DEL' = (calc, backspace acc)
parseChar calc acc '\BS' = (calc, backspace acc)
parseChar calc acc '\n' = case consumeChar calc acc '\n' of (c, _) -> (c, "")
parseChar calc acc ' ' = case consumeChar calc acc ' ' of (c, _) -> (c, "")
parseChar calc acc c = consumeChar calc acc c

showCalculator :: Calculator a => a -> [Char] -> IO()
showCalculator calc acc = do
    putStr "\ESC[1J\ESC[H"
    calcDisplay calc
    putStr ("> " ++ acc)

doCalculator :: (Calculator a, ConvertibleCalculator a) => a -> [Char] -> [Char] -> IO ()
doCalculator _ _ [] = return ()
doCalculator initialCalc acc (x:xs) =
    let (newCalc, newAcc) = parseChar initialCalc acc x
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

startCalculator :: (Calculator a, ConvertibleCalculator a) => a -> [Char] -> IO ()
startCalculator calc input = do
    showCalculator calc ""
    doCalculator calc "" input

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- getContents
    let initialCalc = defaultCalculator
    withRawInput 0 0 $ startCalculator initialCalc input
