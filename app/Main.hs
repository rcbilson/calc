module Main where

import qualified Data.Map.Strict as Map
import Data.Bits
import Data.Word
import Text.Printf

{- from unix library -}
import System.Posix.Terminal
import System.Posix.IO (stdInput)

{- from base -}
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Control.Exception (finally)

{- https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
 - run an application in raw input / non-canonical mode with given
 - VMIN and VTIME settings. for a description of these, see:
 - http://www.gnu.org/software/libc/manual/html_node/Noncanonical-Input.html
 - as well as `man termios`.
 -}
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime application = do

  {- retrieve current settings -}
  oldTermSettings <- getTerminalAttributes stdInput

  {- modify settings -}
  let newTermSettings =
        flip withoutMode  EnableEcho   . -- don't echo keystrokes
        flip withoutMode  ProcessInput . -- turn on non-canonical mode
        flip withTime     vtime        . -- wait at most vtime decisecs per read
        flip withMinInput vmin         $ -- wait for >= vmin bytes per read
        oldTermSettings

  {- install new settings -}
  setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  application
    `finally` setTerminalAttributes stdInput oldTermSettings Immediately


data Stack a = Stack a a a a
  deriving Show

stackOp1 :: (a -> a) -> Engine a b -> Engine a b
stackOp1 f (Engine (Stack x y z t) ops) = Engine (Stack (f x) y z t) ops
stackOp2 :: Num a => (a -> a -> a) -> Engine a b -> Engine a b
stackOp2 f (Engine (Stack x y z t) ops) = Engine (Stack (f y x) z t 0) ops
opStateOp :: (b -> b) -> Engine a b -> Engine a b
opStateOp f (Engine stk ops) = Engine stk (f ops)

data Base = BaseDec | BaseHex | BaseBin deriving Show
data OpStateInteger = OpStateInteger
    { base :: Base
    } deriving Show
data OpStateFloat = OpStateFloat
    { prec :: Maybe Int
    , width :: Maybe Int
    } deriving Show
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

push :: Engine a b -> a -> Engine a b
push (Engine (Stack x y z _) ops) q = Engine (Stack q x y z) ops

dup :: Engine a b -> Engine a b
dup (Engine (Stack x y z _) ops) = Engine (Stack x x y z) ops

swap :: Engine a b -> Engine a b
swap (Engine (Stack x y z t) ops) = Engine (Stack y x z t) ops

rot :: Engine a b -> Engine a b
rot (Engine (Stack x y z t) ops) = Engine (Stack y z t x) ops

numericOps :: Num a => [(String, Engine a b -> Engine a b)]
numericOps =
        [ ("+", stackOp2(+))
        , ("-", stackOp2(-))
        , ("*", stackOp2(*))
        , ("neg", stackOp1 negate)
        , ("dup", dup)
        , ("swap", swap)
        , ("rot", rot)
        ]

setp :: Engine Float OpStateFloat -> Engine Float OpStateFloat
setp (Engine (Stack x y z t) ops) = Engine (Stack y z t 0) ops{prec=Just $ floor x}

setw :: Engine Float OpStateFloat -> Engine Float OpStateFloat
setw (Engine (Stack x y z t) ops) = Engine (Stack y z t 0) ops{width=Just $ floor x}

floatOps :: [(String, Engine Float OpStateFloat -> Engine Float OpStateFloat)]
floatOps = numericOps ++
        [ ("/", stackOp2(/))
        , ("clearp", opStateOp (\s -> s{prec = Nothing}))
        , ("setp", setp)
        , ("clearw", opStateOp (\s -> s{width = Nothing}))
        , ("setw", setw)
        ] 

displayFloat :: Engine Float OpStateFloat -> IO ()
displayFloat (Engine (Stack x y z t) ops) = do
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

bin :: Integral a => Engine a OpStateInteger -> Engine a OpStateInteger
bin (Engine stk ops) = Engine stk ops{base=BaseBin}

dec :: Integral a => Engine a OpStateInteger -> Engine a OpStateInteger
dec (Engine stk ops) = Engine stk ops{base=BaseDec}

hex :: Integral a => Engine a OpStateInteger -> Engine a OpStateInteger
hex (Engine stk ops) = Engine stk ops{base=BaseHex}

intOps :: (Integral a, Bits a) => [(String, Engine a OpStateInteger -> Engine a OpStateInteger)]
intOps = numericOps ++
        [ ("bin", bin)
        , ("dec", dec)
        , ("hex", hex)
        , ("/", stackOp2 div)
        , ("%", stackOp2 mod)
        , ("&", stackOp2(.&.))
        , ("|", stackOp2(.|.))
        , ("^", stackOp2 xor)
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

remainders :: Integral a => a -> a -> [a]
remainders _ 0 = []
remainders divisor dividend = (dividend `mod` divisor):(remainders divisor $ dividend `div` divisor)

chunk :: Int -> [Char] -> [Char]
chunk n xs
    | length xs > n = (take n xs) ++ " " ++ (chunk n $ drop n xs)
    | otherwise     = xs

formatNoWsize :: Integral a => a -> Int -> a -> [Char]
formatNoWsize b c v
    | v == 0    = "0"
    | v < 0     = '-':(formatNoWsize b c (-v))
    | otherwise = (reverse . chunk c . map intDigit) $ remainders b v

formatWsize :: Integral a => Int -> a -> Int -> a -> [Char]
formatWsize w b c v
  | n < w     = reverse $ chunk c $ f ++ (replicate (w-n) '0')
  | otherwise = reverse $ chunk c $ f
  where
    f = map intDigit $ remainders b v
    n = length f

displayIntegral :: Integral a => (a -> [Char]) -> Engine a OpStateInteger -> IO ()
displayIntegral format (Engine (Stack x y z t) ops) = do
    putStrLn $ show ops
    putStrLn $ "t " ++ (format t)
    putStrLn $ "z " ++ (format z)
    putStrLn $ "y " ++ (format y)
    putStrLn $ "x " ++ (format x)

displayInteger :: Integral a => Engine a OpStateInteger -> IO ()
displayInteger eng@(Engine _ ops) =
    let (b, c) = case (base ops) of
            BaseBin -> (2, 4)
            BaseDec -> (10, 3)
            BaseHex -> (16, 4)
        format = formatNoWsize b c
    in displayIntegral format eng

displayFixed :: (Integral a, FiniteBits a) => Engine a OpStateInteger -> IO ()
displayFixed eng@(Engine (Stack x _ _ _) ops) =
    let (b, c, w) = case (base ops) of
            BaseBin -> (2, 4, (finiteBitSize x))
            BaseDec -> (10, 3, 1)
            BaseHex -> (16, 4, (finiteBitSize x) `div` 4)
        format = formatWsize w b c
    in displayIntegral format eng

floatCalculator :: Calculator Float OpStateFloat
floatCalculator = Calculator
    { engine = Engine
        { stack = Stack 0 0 0 0
        , opState = OpStateFloat{ prec = Nothing, width = Nothing }
        }
    , opsMap = Map.fromList floatOps
    , readNum = reads :: String -> [(Float,String)] 
    , display = displayFloat
    }

intCalculator :: Calculator Integer OpStateInteger
intCalculator = Calculator
    { engine = Engine
        { stack = Stack 0 0 0 0
        , opState = OpStateInteger { base = BaseDec }
        }
    , opsMap = Map.fromList intOps
    , readNum = reads :: String -> [(Integer,String)] 
    , display = displayInteger
    }

readFixed :: (Read a, FiniteBits a) => String -> [(a,String)] 
readFixed = reads

fixedCalculator :: (Integral a, FiniteBits a, Read a) => Calculator a OpStateInteger
fixedCalculator = Calculator
    { engine = Engine
        { stack = Stack 0 0 0 0
        , opState = OpStateInteger { base = BaseDec }
        }
    , opsMap = Map.fromList intOps
    , readNum = readFixed
    , display = displayFixed
    }

data Token a = Op String | Num a deriving Show

consumeToken :: Calculator a b -> Token a -> Calculator a b
consumeToken calc token =
    case token of
        Op op -> case Map.lookup op (opsMap calc) of
            Just f -> calc{engine=f (engine calc)}
            Nothing -> error ("Unknown operator: " ++ op)
        Num n -> calc{engine=push (engine calc) n}

tryParse :: Calculator a b -> [Char] -> (Maybe (Token a), [Char])
tryParse _ [] = (Nothing, [])
tryParse calc str =
    -- Special case: if it's 0x or 0X it could be the beginning of
    -- a valid hex number so let it ride.
    case str of
      "0x" -> (Nothing, str)
      "0X" -> (Nothing, str)
      "_" -> (Nothing, "-")
      _ -> case (readNum calc) str of
        (num, rest):_ ->
            -- Special case: if it's a valid number with a period following
            -- it could continue on to be some real number, so continue
            -- accumulating characters.
            if rest == "." then (Nothing, str)
            else (Just (Num num), rest)
        [] ->
            if Map.member str (opsMap calc) then (Just (Op str), [])
            else (Nothing, str)

parseChar :: Calculator a b -> [Char] -> Char -> (Maybe (Token a), [Char])
parseChar calc acc ' ' = case tryParse calc acc of (t, _) -> (t, [])
parseChar calc acc '\n' = case tryParse calc acc of (t, _) -> (t, [])
parseChar _ [] '\DEL' = (Nothing, [])
parseChar _ acc '\DEL' =
    case reverse acc of
        [] -> (Nothing, [])
        _:xs -> (Nothing, reverse xs)
parseChar _ [] '\BS' = (Nothing, [])
parseChar _ acc '\BS' =
    case reverse acc of
        [] -> (Nothing, [])
        _:xs -> (Nothing, reverse xs)
parseChar calc acc c =
    let
        newacc = reverse (c:(reverse acc))
    in case tryParse calc newacc of
        -- special case: if we parsed the complete input as a number, don't return a token:
        -- it may be the case that the next character extends the number
        (Just (Num _), []) -> (Nothing, newacc)
        (t, r) -> (t, r)

showCalculator :: (Show a, Show b) => Calculator a b -> [Char] -> IO()
showCalculator calc acc = do
    putStr "\ESC[1J\ESC[H"
    (display calc) (engine calc)
    putStr ("> " ++ acc)

doCalculator :: (Show a, Show b, Read a) => Calculator a b -> [Char] -> [Char] -> IO ()
doCalculator _ _ [] = return ()
doCalculator initialCalc acc (x:xs) =
    let (newCalc, newAcc) = case parseChar initialCalc acc x of
            (Nothing, rest) -> (initialCalc, rest)
            (Just t, rest) -> 
                -- special case: if the character caused a token to be returned, it might
                -- be the case that the remainder is also a valid token (consider the input
                -- "123+", the + causes the number to be completed as a token but it is
                -- itself a token.
                let
                    calc2 = consumeToken initialCalc t
                in
                    case tryParse calc2 rest of
                        (Nothing, rest2) -> (calc2, rest2)
                        (Just t2, rest2) -> (consumeToken calc2 t2, rest2)
    in case newAcc of 
        "\\x" -> return ()
        "\\f" -> startCalculator floatCalculator xs
        "\\i" -> startCalculator intCalculator xs
        "\\8" -> startCalculator (fixedCalculator :: Calculator Word8 OpStateInteger) xs
        "\\16" -> startCalculator (fixedCalculator :: Calculator Word16 OpStateInteger) xs
        "\\32" -> startCalculator (fixedCalculator :: Calculator Word32 OpStateInteger) xs
        "\\64" -> startCalculator (fixedCalculator :: Calculator Word64 OpStateInteger) xs
        _ -> do
            showCalculator newCalc newAcc
            doCalculator newCalc newAcc xs

startCalculator :: (Show a, Show b, Read a) => Calculator a b -> [Char] -> IO ()
startCalculator calc input = do
    showCalculator calc ""
    doCalculator calc "" input

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- getContents
    withRawInput 0 0 $ startCalculator intCalculator input
