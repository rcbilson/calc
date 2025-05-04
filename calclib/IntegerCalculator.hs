module IntegerCalculator ( defaultCalculator,
    opStateIntegerDefault,
    IntegerCalculator(IntegerCalculator),
    Word8Calculator(Word8Calculator),
    Word16Calculator(Word16Calculator),
    Word32Calculator(Word32Calculator),
    Word64Calculator(Word64Calculator) ) where

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
toggleChunk :: Engine a OpStateInteger -> Engine a OpStateInteger
toggleChunk (Engine stk ops) = case chunk ops of
    NoChunk -> Engine stk ops{chunk=Chunk}
    Chunk   -> Engine stk ops{chunk=NoChunk}

intOps :: (Integral a, Bits a) => Map.Map String (Engine a OpStateInteger -> Engine a OpStateInteger)
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
        , ("shift", stackOp2 $ placeValueOp shift)
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

-- displayIntegral displays the state of an integral calculator.
-- It displays the opState and then uses the given formatter function
-- to display the top four stack entries.
displayIntegral :: Integral a => (a -> [Char]) -> Engine a OpStateInteger -> IO ()
displayIntegral format (Engine (x:y:z:t:_) ops) = do
    putStrLn $ show ops
    putStrLn $ "t " ++ (format t)
    putStrLn $ "z " ++ (format z)
    putStrLn $ "y " ++ (format y)
    putStrLn $ "x " ++ (format x)
displayIntegral _ _ = error("displayIntegral underflow")

-- displayInteger displays the state of an integral calculator without
-- a constraint on the word size.
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

-- IntegerCalculator holds the state of an infinite-precision integer calculator
data IntegerCalculator = IntegerCalculator (Engine Integer OpStateInteger)

-- floatConsume attempts to find a prefix of the given string that is one of
-- the defined integer operations or can be parsed as a number using the given
-- function. It updates the engine if one is found, and then returns the
-- updated engine and the remainder of the string.
intConsume :: (Integral a, Bits a) => (String -> [(a, String)]) -> Engine a OpStateInteger -> String -> (Engine a OpStateInteger, String)
intConsume = genericConsume (flip Map.lookup intOps)

instance Calculator IntegerCalculator where
    calcDisplay (IntegerCalculator engine) = displayInteger engine
    calcConsume (IntegerCalculator engine) str = let (eng, rest) = intConsume reads engine str in (IntegerCalculator(eng), rest)

-- defaultCalculator is the kind of calculator used when the program starts.
defaultCalculator :: IntegerCalculator
defaultCalculator = IntegerCalculator (Engine [0,0,0,0] opStateIntegerDefault)

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

-- Word8Calculator holds the state of a calculator that operates on Word8s
data Word8Calculator = Word8Calculator (Engine Word8 OpStateInteger)

instance Calculator Word8Calculator where
    calcDisplay (Word8Calculator engine) = displayFixed engine
    calcConsume (Word8Calculator engine) str = let (eng, rest) = intConsume reads engine str in (Word8Calculator eng, rest)

-- Word16Calculator holds the state of a calculator that operates on Word16s
data Word16Calculator = Word16Calculator (Engine Word16 OpStateInteger)

instance Calculator Word16Calculator where
    calcDisplay (Word16Calculator engine) = displayFixed engine
    calcConsume (Word16Calculator engine) str = let (eng, rest) = intConsume reads engine str in (Word16Calculator eng, rest)

-- Word32Calculator holds the state of a calculator that operates on Word32s
data Word32Calculator = Word32Calculator (Engine Word32 OpStateInteger)

instance Calculator Word32Calculator where
    calcDisplay (Word32Calculator engine) = displayFixed engine
    calcConsume (Word32Calculator engine) str = let (eng, rest) = intConsume reads engine str in (Word32Calculator eng, rest)

-- Word64Calculator holds the state of a calculator that operates on Word64s
data Word64Calculator = Word64Calculator (Engine Word64 OpStateInteger)

instance Calculator Word64Calculator where
    calcDisplay (Word64Calculator engine) = displayFixed engine
    calcConsume (Word64Calculator engine) str = let (eng, rest) = intConsume reads engine str in (Word64Calculator eng, rest)
