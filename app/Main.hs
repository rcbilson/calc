module Main where

import qualified Data.Map.Strict as Map
import Data.Bits
import Data.Word
import Text.Printf
import WithRawInput
import IntegerCalculator
import Calculator
import ConvertibleCalculator

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
{-
type Stack a = [a]

ensureStack :: Num a => Stack a -> Stack a
ensureStack stk
    | length(stk) < 4 = ensureStack (stk ++ [0])
    | otherwise       = stk

stackOp1 :: (a -> a) -> Engine a b -> Engine a b
stackOp1 f (Engine (x:xs) ops) = Engine ((f x):xs) ops
stackOp1 _ _ = error("stackOp1 underflow")
stackOp2 :: Num a => (a -> a -> a) -> Engine a b -> Engine a b
stackOp2 f (Engine (x:y:xs) ops) = Engine (ensureStack $ (f y x):xs) ops
stackOp2 _ _ = error("stackOp2 underflow")
opStateOp :: (b -> b) -> Engine a b -> Engine a b
opStateOp f (Engine stk ops) = Engine stk (f ops)

class CalcType a where
    toFloat :: a -> Float
    toInt :: a -> Integer

data Engine a b = Engine
    { stack :: Stack a
    , opState :: b
    } deriving Show
data Calculator a b = Calculator
    { engine :: Engine a b
    , opsMap :: Map.Map String (Engine a b -> Engine a b)
    , readNum :: String -> [(a,String)]
    , display :: Engine a b -> IO ()
    }

instance (Show a, Show b) => Show (Calculator a b) where
    show calc = show (engine calc)

class OpState a where
    toOpStateInteger :: a -> OpStateInteger
    toOpStateFloat :: a -> OpStateFloat

push :: Engine a b -> a -> Engine a b
push (Engine stk ops) q = Engine (q:stk) ops

dup :: Engine a b -> Engine a b
dup (Engine (x:xs) ops) = Engine (x:x:xs) ops
dup _ = error("dup underflow")

swap :: Engine a b -> Engine a b
swap (Engine (x:y:xs) ops) = Engine (y:x:xs) ops
swap _ = error("swap underflow")

drop1 :: Engine a b -> Engine a b
drop1 (Engine (_:xs) ops) = Engine xs ops
drop1 _ = error("drop1 underflow")

numericOps :: Num a => [(String, Engine a b -> Engine a b)]
numericOps =
        [ ("+", stackOp2(+))
        , ("-", stackOp2(-))
        , ("*", stackOp2(*))
        , ("neg", stackOp1 negate)
        , ("dup", dup)
        , ("swap", swap)
        , ("drop", drop1)
        ]

---------------- Floating-point calculator --------------------------
instance CalcType Float where
    toFloat x = x
    toInt x = floor x

data OpStateFloat = OpStateFloat
    { prec :: Maybe Int
    , width :: Maybe Int
    } deriving Show

opStateFloatDefault :: OpStateFloat
opStateFloatDefault = OpStateFloat{ prec = Nothing, width = Nothing }

instance OpState OpStateFloat where
    toOpStateInteger _ = opStateIntegerDefault
    toOpStateFloat = id

setp :: Engine Float OpStateFloat -> Engine Float OpStateFloat
setp (Engine (x:xs) ops) = Engine (ensureStack xs) ops{prec=Just $ floor x}
setp _ = error("setp underflow")

setw :: Engine Float OpStateFloat -> Engine Float OpStateFloat
setw (Engine (x:xs) ops) = Engine (ensureStack xs) ops{width=Just $ floor x}
setw _ = error("setw underflow")

floatOps :: [(String, Engine Float OpStateFloat -> Engine Float OpStateFloat)]
floatOps = numericOps ++
        [ ("/", stackOp2(/))
        , ("^", stackOp2(**))
        , ("clearp", opStateOp (\s -> s{prec = Nothing}))
        , ("setp", setp)
        , ("clearw", opStateOp (\s -> s{width = Nothing}))
        , ("setw", setw)
        ] 

displayFloat :: Engine Float OpStateFloat -> IO ()
displayFloat (Engine (x:y:z:t:_) ops) = do
    let format = case ((width ops), (prec ops)) of
            (Nothing, Nothing) -> "%g"
            (Just w, Nothing) -> printf "%%%dg" w
            (Nothing, Just p) -> printf "%%.%dg" p
            (Just w, Just p) -> printf "%%%d.%dg" w p
    putStrLn $ show ops
    printf ("t " ++ format ++ "\n") t
    printf ("z " ++ format ++ "\n") z
    printf ("y " ++ format ++ "\n") y
    printf ("x " ++ format ++ "\n") x
displayFloat _ = error("displayFloat underflow")
floatCalculator :: Stack Float -> OpStateFloat -> Calculator Float OpStateFloat
floatCalculator initialStack initialState = Calculator
    { engine = Engine
        { stack = initialStack
        , opState = initialState
        }
    , opsMap = Map.fromList floatOps
    , readNum = reads :: String -> [(Float,String)] 
    , display = displayFloat
    }

convertToFloat :: (CalcType a, OpState b) => Calculator a b -> Calculator Float OpStateFloat
convertToFloat calc =
    let
        oldStack = stack $ engine $ calc
        newStack = fmap toFloat oldStack
        oldState = opState $ engine $ calc
        newState = toOpStateFloat oldState
    in
        floatCalculator newStack newState
-}
---------------- The Calculator Interface --------------------------

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
--        "\\f" -> startCalculator (convertToFloat newCalc) xs
        "\\i" -> startCalculator (calcToInteger newCalc) xs
        "\\8" -> startCalculator (calcToWord8 newCalc) xs
--        "\\16" -> startCalculator (convertToIntegral newCalc fixedCalculator :: Calculator Word16 OpStateInteger) xs
--        "\\32" -> startCalculator (convertToIntegral newCalc fixedCalculator :: Calculator Word32 OpStateInteger) xs
--        "\\64" -> startCalculator (convertToIntegral newCalc fixedCalculator :: Calculator Word64 OpStateInteger) xs
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
    let initialCalc = newIntegerCalculator
    withRawInput 0 0 $ startCalculator initialCalc input
