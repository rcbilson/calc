module Main where

import qualified Data.Map.Strict as Map
import Data.Bits
import Data.Word
import Text.Printf
import WithRawInput
import IntCalculator
import CalculatorClass

import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

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

---------------- Integer calculators --------------------------

data Base = BaseDec | BaseHex | BaseBin deriving Show
data Chunk = Chunk | NoChunk deriving Show
data OpStateInteger = OpStateInteger
    { base :: Base
    , chunk :: Chunk
    } deriving Show

instance OpState OpStateInteger where
    toOpStateInteger = id
    toOpStateFloat _ = opStateFloatDefault

opStateIntegerDefault :: OpStateInteger
opStateIntegerDefault = OpStateInteger { base = BaseDec, chunk = Chunk }
instance CalcType Integer where
    toFloat x = fromIntegral x
    toInt x = x

instance CalcType Word8 where
    toFloat x = fromIntegral x
    toInt x = fromIntegral x

instance CalcType Word16 where
    toFloat x = fromIntegral x
    toInt x = fromIntegral x

instance CalcType Word32 where
    toFloat x = fromIntegral x
    toInt x = fromIntegral x

instance CalcType Word64 where
    toFloat x = fromIntegral x
    toInt x = fromIntegral x

bin :: Integral a => Engine a OpStateInteger -> Engine a OpStateInteger
bin (Engine stk ops) = Engine stk ops{base=BaseBin}

dec :: Integral a => Engine a OpStateInteger -> Engine a OpStateInteger
dec (Engine stk ops) = Engine stk ops{base=BaseDec}

hex :: Integral a => Engine a OpStateInteger -> Engine a OpStateInteger
hex (Engine stk ops) = Engine stk ops{base=BaseHex}

toggleChunk :: Engine a OpStateInteger -> Engine a OpStateInteger
toggleChunk (Engine stk ops) = case chunk ops of
    NoChunk -> Engine stk ops{chunk=Chunk}
    Chunk   -> Engine stk ops{chunk=NoChunk}

intOps :: (Integral a, Bits a) => [(String, Engine a OpStateInteger -> Engine a OpStateInteger)]
intOps = numericOps ++
        [ ("bin", bin)
        , ("dec", dec)
        , ("hex", hex)
        , ("chunk", toggleChunk)
        , ("^", stackOp2(^))
        , ("/", stackOp2 div)
        , ("%", stackOp2 mod)
        , ("&", stackOp2(.&.))
        , ("|", stackOp2(.|.))
        , ("~", stackOp2 xor)
        , ("!", stackOp1 complement)
        , ("shift", stackOp2 $ placeValueOp shift)
        , ("<", stackOp1 $ (flip shift) 1)
        , (">", stackOp1 $ (flip shift) (-1))
        , ("sb", stackOp2 $ placeValueOp setBit)
        , ("cb", stackOp2 $ placeValueOp clearBit)
        ]
    where
        placeValueOp op val place = op val (fromIntegral place)

intDigit :: Integral a => a -> Char
intDigit 0 = '0'
intDigit 1 = '1'
intDigit 2 = '2'
intDigit 3 = '3'
intDigit 4 = '4'
intDigit 5 = '5'
intDigit 6 = '6'
intDigit 7 = '7'
intDigit 8 = '8'
intDigit 9 = '9'
intDigit 10 = 'a'
intDigit 11 = 'b'
intDigit 12 = 'c'
intDigit 13 = 'd'
intDigit 14 = 'e'
intDigit 15 = 'f'
intDigit _  = error("invalid digit")

remainders :: Integral a => a -> a -> [a]
remainders _ 0 = []
remainders divisor dividend = (dividend `mod` divisor):(remainders divisor $ dividend `div` divisor)

chunkDigits :: Int -> [Char] -> [Char]
chunkDigits n xs
    | length xs > n = (take n xs) ++ " " ++ (chunkDigits n $ drop n xs)
    | otherwise     = xs

formatNoWsize :: Integral a => a -> ([Char] -> [Char]) -> a -> [Char]
formatNoWsize b c v
    | v == 0    = "0"
    | v < 0     = '-':(formatNoWsize b c (-v))
    | otherwise = (reverse . c . map intDigit) $ remainders b v

formatWsize :: Integral a => Int -> a -> ([Char] -> [Char]) -> a -> [Char]
formatWsize w b c v
  | n < w     = reverse $ c $ f ++ (replicate (w-n) '0')
  | otherwise = reverse $ c $ f
  where
    f = map intDigit $ remainders b v
    n = length f

displayIntegral :: Integral a => (a -> [Char]) -> Engine a OpStateInteger -> IO ()
displayIntegral format (Engine (x:y:z:t:_) ops) = do
    putStrLn $ show ops
    putStrLn $ "t " ++ (format t)
    putStrLn $ "z " ++ (format z)
    putStrLn $ "y " ++ (format y)
    putStrLn $ "x " ++ (format x)
displayIntegral _ _ = error("displayIntegral underflow")

displayInteger :: Integral a => Engine a OpStateInteger -> IO ()
displayInteger eng@(Engine _ ops) =
    let (b, c) = case (base ops) of
            BaseBin -> (2, 4)
            BaseDec -> (10, 3)
            BaseHex -> (16, 4)
        chunkFn = case (chunk ops) of
            Chunk -> chunkDigits c
            NoChunk -> id
        format = formatNoWsize b chunkFn
    in displayIntegral format eng

displayFixed :: (Integral a, FiniteBits a) => Engine a OpStateInteger -> IO ()
displayFixed eng@(Engine (x:_) ops) =
    let (b, c, w) = case (base ops) of
            BaseBin -> (2, 4, (finiteBitSize x))
            BaseDec -> (10, 3, 1)
            BaseHex -> (16, 4, (finiteBitSize x) `div` 4)
        chunkFn = case (chunk ops) of
            Chunk -> chunkDigits c
            NoChunk -> id
        format = formatWsize w b chunkFn
    in displayIntegral format eng
displayFixed _ = error("displayFixed underflow")

intCalculator :: Stack Integer -> OpStateInteger -> Calculator Integer OpStateInteger
intCalculator initialStack initialState = Calculator
    { engine = Engine
        { stack = initialStack
        , opState = initialState
        }
    , opsMap = Map.fromList intOps
    , readNum = reads :: String -> [(Integer,String)] 
    , display = displayInteger
    }

readFixed :: (Read a, FiniteBits a) => String -> [(a,String)] 
readFixed = reads

fixedCalculator :: (Integral a, FiniteBits a, Read a) => Stack a -> OpStateInteger -> Calculator a OpStateInteger
fixedCalculator initialStack initialState = Calculator
    { engine = Engine
        { stack = initialStack
        , opState = initialState
        }
    , opsMap = Map.fromList intOps
    , readNum = readFixed
    , display = displayFixed
    }

convertToIntegral :: (CalcType a, Integral b, OpState c) => Calculator a c -> (Stack b -> OpStateInteger -> Calculator b OpStateInteger) -> Calculator b OpStateInteger
convertToIntegral calc construct = 
    let
        oldStack = stack $ engine $ calc
        newStack = fmap (fromIntegral . toInt) oldStack
        oldState = opState $ engine $ calc
        newState = toOpStateInteger oldState
    in
        construct newStack newState

---------------- The Calculator Interface --------------------------

backspace :: String -> String
backspace [] = []
backspace acc =
    case reverse acc of
        [] -> []
        _:xs -> reverse xs

consumeChar :: CalculatorClass a => a -> [Char] -> Char -> (a, [Char])
consumeChar calc acc c =
    let
        newacc = reverse (c:(reverse acc))
    in calcConsume calc newacc

parseChar :: CalculatorClass a => a -> [Char] -> Char -> (a, [Char])
parseChar calc acc '\DEL' = (calc, backspace acc)
parseChar calc acc '\BS' = (calc, backspace acc)
parseChar calc acc '\n' = case consumeChar calc acc '\n' of (c, _) -> (c, "")
parseChar calc acc ' ' = case consumeChar calc acc ' ' of (c, _) -> (c, "")
parseChar calc acc c = consumeChar calc acc c

showCalculator :: CalculatorClass a => a -> [Char] -> IO()
showCalculator calc acc = do
    putStr "\ESC[1J\ESC[H"
    calcDisplay calc
    putStr ("> " ++ acc)

doCalculator :: CalculatorClass a => a -> [Char] -> [Char] -> IO ()
doCalculator _ _ [] = return ()
doCalculator initialCalc acc (x:xs) =
    let (newCalc, newAcc) = parseChar initialCalc acc x
    in case newAcc of 
        "\\x" -> return ()
--        "\\f" -> startCalculator (convertToFloat newCalc) xs
--        "\\i" -> startCalculator (convertToIntegral newCalc intCalculator) xs
--        "\\8" -> startCalculator (convertToIntegral newCalc fixedCalculator :: Calculator Word8 OpStateInteger) xs
--        "\\16" -> startCalculator (convertToIntegral newCalc fixedCalculator :: Calculator Word16 OpStateInteger) xs
--        "\\32" -> startCalculator (convertToIntegral newCalc fixedCalculator :: Calculator Word32 OpStateInteger) xs
--        "\\64" -> startCalculator (convertToIntegral newCalc fixedCalculator :: Calculator Word64 OpStateInteger) xs
        _ -> do
            showCalculator newCalc newAcc
            doCalculator newCalc newAcc xs

startCalculator :: CalculatorClass a => a -> [Char] -> IO ()
startCalculator calc input = do
    showCalculator calc ""
    doCalculator calc "" input

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- getContents
    let initialCalc = newIntCalculator
    withRawInput 0 0 $ startCalculator initialCalc input
