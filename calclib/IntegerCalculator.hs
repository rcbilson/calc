module IntegerCalculator (
    newIntegerCalculator,
    newWord8Calculator,
    newWord16Calculator,
    newWord32Calculator,
    newWord64Calculator,
    testIntegerCalculator,
    testWord8Calculator,
    testWord16Calculator,
    testWord32Calculator,
    testWord64Calculator,
    OpStateInteger,
    opStateIntegerDefault) where

import Calculator
import qualified Data.Map.Strict as Map
import Data.Bits
import Data.Word

-- OpStateInteger is the operational state for the integral calculators.
--   - base: display base (2, 10, 16)
--   - chunk: whether to separate groups of display digits with spaces
--     (e.g., 12000 or 12 000)
data Base = BaseDec | BaseHex | BaseBin deriving Show
data Chunk = Chunk | NoChunk deriving Show
data OpStateInteger = OpStateInteger
    { base :: Base
    , chunk :: Chunk
    } deriving Show

-- opStateIntegerDefault is the operational state that an integral calculator
-- should start with.
opStateIntegerDefault :: OpStateInteger
opStateIntegerDefault = OpStateInteger { base = BaseDec, chunk = Chunk }

-- toggleChunk toggles the state of output chunking.
toggleChunk :: EngineFn a OpStateInteger
toggleChunk (Engine stk ops) = case chunk ops of
    NoChunk -> (Engine stk ops{chunk=Chunk}, NoUndo)
    Chunk   -> (Engine stk ops{chunk=NoChunk}, NoUndo)

intOps :: (Integral a, Bits a) => Map.Map String (EngineFn a OpStateInteger)
intOps = Map.fromList $ numericOps ++
        [ ("bin", opStateOp (\s -> s{base = BaseBin}))
        , ("dec", opStateOp (\s -> s{base = BaseDec}))
        , ("hex", opStateOp (\s -> s{base = BaseHex}))
        , ("chunk", toggleChunk)
        , ("^", stackOp2(^))
        , ("/", stackOp2 div)
        , ("%", stackOp2 mod)
        , ("&", stackOp2(.&.))
        , ("|", stackOp2(.|.))
        , ("~", stackOp2 xor)
        , ("!", stackOp1 complement)
        , ("shl", stackOp2 $ placeValueOp shiftL)
        , ("shr", stackOp2 $ placeValueOp shiftR)
        , ("<", stackOp1 $ (flip shift) 1)
        , (">", stackOp1 $ (flip shift) (-1))
        , ("sb", stackOp2 $ placeValueOp setBit)
        , ("cb", stackOp2 $ placeValueOp clearBit)
        ]
    where
        -- Some of the operations require an Int to indicate a digit position.
        -- We want to take that from the stack but we need to convert from
        -- whatever the stack type is to Int.
        placeValueOp op val place = op val (fromIntegral place)

-- intDigit maps from a number to a corresponding ASCII digit.
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

-- remainders produces a list of remainders of repeated division of the
-- given divisor by the given dividend. The head of the list is the
-- first remainder so obtained (i.e., it represents the value of the
-- least-significant digit).
remainders :: Integral a => a -> a -> [a]
remainders _ 0 = []
remainders divisor dividend = (dividend `mod` divisor):(remainders divisor $ dividend `div` divisor)

-- chunkDigits takes a string and inserts spaces every n characters
-- starting from the beginning of the string.
chunkDigits :: Int -> [Char] -> [Char]
chunkDigits n xs
    | length xs > n = (take n xs) ++ " " ++ (chunkDigits n $ drop n xs)
    | otherwise     = xs

-- formatNoWsize formats an infinite-precision integer for display.
formatNoWsize :: Integral a => a -> ([Char] -> [Char]) -> a -> [Char]
formatNoWsize b c v
    | v == 0    = "0"
    | v < 0     = '-':(formatNoWsize b c (-v))
    | otherwise = reverse $ c $ map intDigit $ remainders b v

displayOps :: OpStateInteger -> String
displayOps ops =
    let
        chunkStr = case (chunk ops) of
            NoChunk -> " nochunk"
            _ -> ""
        baseStr = case (base ops) of
            BaseBin -> " bin"
            BaseHex -> " hex"
            BaseDec -> " dec"
    in
        baseStr ++ chunkStr

-- displayIntegral displays the state of an integral calculator.
-- It displays the opState and then uses the given formatter function
-- to display the top four stack entries.
displayIntegral :: Integral a => [Char] -> (a -> [Char]) -> Engine a OpStateInteger -> IO ()
displayIntegral name format (Engine (x:y:z:t:_) ops) = do
    putStrLn $ name ++ (displayOps ops)
    putStrLn $ "t " ++ (format t)
    putStrLn $ "z " ++ (format z)
    putStrLn $ "y " ++ (format y)
    putStrLn $ "x " ++ (format x)
displayIntegral _ _ _ = error("displayIntegral underflow")

-- displayInteger displays the state of an integral calculator without
-- a constraint on the word size.
displayInteger :: Integral a => [Char] -> Engine a OpStateInteger -> IO ()
displayInteger title eng@(Engine _ ops) =
    let (b, c) = case (base ops) of
            BaseBin -> (2, 4)
            BaseDec -> (10, 3)
            BaseHex -> (16, 4)
        chunkFn = case (chunk ops) of
            Chunk -> chunkDigits c
            NoChunk -> id
        format = formatNoWsize b chunkFn
    in displayIntegral title format eng

newIntegerCalculator :: Stack Integer -> OpStateInteger -> Calculator Integer OpStateInteger
newIntegerCalculator stk ops = Calculator
    { calcEngine = Engine stk ops
    , calcUndos = []
    , calcRedos = []
    , calcOp = flip Map.lookup intOps
    , calcReads = reads
    , calcDisp = displayInteger "Integer" . calcEngine
    }

testIntegerCalculator :: Calculator Integer OpStateInteger
testIntegerCalculator = newIntegerCalculator [0,0,0,0] opStateIntegerDefault

---------------------- Fixed-width ---------------------------

-- formatNoWsize formats an fixed-word-sized integer for display.
formatWsize :: Integral a => Int -> a -> ([Char] -> [Char]) -> a -> [Char]
formatWsize w b c v
  | n < w     = reverse $ c $ f ++ (replicate (w-n) '0')
  | otherwise = reverse $ c $ f
  where
    f = map intDigit $ remainders b v
    n = length f

-- displayFixed displays the state of an integral calculator with
-- a constraint on the word size.
displayFixed :: (Integral a, FiniteBits a) => [Char] -> Engine a OpStateInteger -> IO ()
displayFixed title eng@(Engine (x:_) ops) =
    let (b, c, w) = case (base ops) of
            BaseBin -> (2, 4, (finiteBitSize x))
            BaseDec -> (10, 3, 1)
            BaseHex -> (16, 4, (finiteBitSize x) `div` 4)
        chunkFn = case (chunk ops) of
            Chunk -> chunkDigits c
            NoChunk -> id
        format = formatWsize w b chunkFn
    in displayIntegral title format eng
displayFixed _ _ = error("displayFixed underflow")

newFixedCalculator :: (Integral a, FiniteBits a, Read a) => String -> Stack a -> OpStateInteger -> Calculator a OpStateInteger
newFixedCalculator lbl stk ops = Calculator
    { calcEngine = Engine stk ops
    , calcUndos = []
    , calcRedos = []
    , calcOp = flip Map.lookup intOps
    , calcReads = reads
    , calcDisp = displayFixed lbl . calcEngine
    }

newWord8Calculator :: Stack Word8 -> OpStateInteger -> Calculator Word8 OpStateInteger
newWord8Calculator = newFixedCalculator "Word8"

newWord16Calculator :: Stack Word16 -> OpStateInteger -> Calculator Word16 OpStateInteger
newWord16Calculator = newFixedCalculator "Word16"

newWord32Calculator :: Stack Word32 -> OpStateInteger -> Calculator Word32 OpStateInteger
newWord32Calculator = newFixedCalculator "Word32"

newWord64Calculator :: Stack Word64 -> OpStateInteger -> Calculator Word64 OpStateInteger
newWord64Calculator = newFixedCalculator "Word64"

testWord8Calculator :: Calculator Word8 OpStateInteger
testWord8Calculator = newWord8Calculator [0,0,0,0] opStateIntegerDefault

testWord16Calculator :: Calculator Word16 OpStateInteger
testWord16Calculator = newWord16Calculator [0,0,0,0] opStateIntegerDefault

testWord32Calculator :: Calculator Word32 OpStateInteger
testWord32Calculator = newWord32Calculator [0,0,0,0] opStateIntegerDefault

testWord64Calculator :: Calculator Word64 OpStateInteger
testWord64Calculator = newWord64Calculator [0,0,0,0] opStateIntegerDefault
