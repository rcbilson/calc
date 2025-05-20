module Runner (startCalculator, defaultCalculator, testCalculator) where

import DoubleCalculator
import Calculator
--import ConvertibleCalculator

-- backspace takes a string and removes the last character
backspace :: String -> String
backspace [] = []
backspace acc =
    case reverse acc of
        [] -> []
        _:xs -> reverse xs

consume :: CalcImpl a b -> String -> (CalcImpl a b, String)
-- Special case: if it's 0x or 0X it could be the beginning of
-- a valid hex number so let it ride.
consume calc "0x" = (calc, "0x")
consume calc "0X" = (calc, "0X")
-- Allow _ to stand in for prefix negation
consume calc "_"  = (calc, "-")
consume calc "u"  = (calcUndo calc, "")
consume calc "U"  = (calcRedo calc, "")
consume calc str = 
    case calcReads calc str of
        -- Special case: if it's a valid number with nothing or a dot
        -- following it could continue on to be a bigger number, so
        -- continue accumulating characters.
        (_, ""):_   -> (calc, str)
        (_, "."):_  -> (calc, str)
        (_, ":"):_  -> (calc, str)
        (num, rest):_ ->
            -- special case: if the character caused a token to be returned, it might
            -- be the case that the remainder is also a valid token (consider the input
            -- "123+", the + causes the number to be completed as a token but it is
            -- itself a token.
            let newCalc = calcApply calc (push num) in consume newCalc rest
        [] -> case calcOp calc str of
            Just f -> (calcApply calc f, "")
            Nothing -> (calc, str)

-- consumeChar adds the character c to the accumulator and attempts
-- to consume a token from the accumulator.
consumeChar :: CalcImpl a b -> [Char] -> Char -> (CalcImpl a b, [Char])
consumeChar calc acc c =
    let
        newacc = reverse (c:(reverse acc))
    in consume calc newacc

-- processChar processes the next input character.
-- It returns the new state of the Calculator and any as-yet-unconsumed
-- input.
processChar :: CalcImpl a b -> [Char] -> Char -> (CalcImpl a b, [Char])
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
showCalculator :: CalcImpl a b -> [Char] -> IO()
showCalculator calc acc = do
    -- ANSI escapes: clear from cursor to beginning of screen, move to position (1,1)
    putStr "\ESC[1J\ESC[H"
    calcDisplay calc
    putStr ("> " ++ acc)

-- testCalculator is a calculator main loop for test purposes. It doesn't have any
-- IO, and it doesn't allow any of the special commands that switch modes so that the
-- return value can be well-typed.
testCalculator :: CalcImpl a b -> [Char] -> [Char] -> CalcImpl a b
testCalculator initialCalc _ [] = initialCalc
testCalculator initialCalc acc (x:xs) =
    let (newCalc, newAcc) = processChar initialCalc acc x
    in case newAcc of
        _ -> testCalculator newCalc newAcc xs

-- doCalculator is the calculator main loop, consuming input characters one at a
-- time and displaying the updated state of the calculator at each step.
-- It also handles certain special operations that can't be expressed as EngineFn.
-- These include exiting the program, and switching to a different arithmetic mode.
doCalculator :: CalcImpl a b -> [Char] -> [Char] -> IO ()
doCalculator _ _ [] = return ()
doCalculator initialCalc acc (x:xs) =
    let (newCalc, newAcc) = processChar initialCalc acc x
    in case newAcc of 
        "\\x" -> return ()
--        "\\f" -> startCalculator (calcToDouble newCalc) xs
--        "\\i" -> startCalculator (calcToInteger newCalc) xs
--        "\\8" -> startCalculator (calcToWord8 newCalc) xs
--        "\\16" -> startCalculator (calcToWord16 newCalc) xs
--        "\\32" -> startCalculator (calcToWord32 newCalc) xs
--        "\\64" -> startCalculator (calcToWord64 newCalc) xs
        _ -> do
            showCalculator newCalc newAcc
            doCalculator newCalc newAcc xs

-- startCalculator displays the initial state of the calculator and then enters
-- the main loop.
startCalculator :: CalcImpl a b -> [Char] -> IO ()
startCalculator calc input = do
    showCalculator calc ""
    doCalculator calc "" input
