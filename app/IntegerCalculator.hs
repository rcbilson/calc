module IntegerCalculator ( newIntegerCalculator ) where

import CalculatorClass
import Data.Bits
import Data.Word
import qualified Data.Map.Strict as Map

data IntegerCalculator = IntegerCalculator { engine :: (Engine Integer OpStateInteger) }

data Base = BaseDec | BaseHex | BaseBin deriving Show
data Chunk = Chunk | NoChunk deriving Show
data OpStateInteger = OpStateInteger
    { base :: Base
    , chunk :: Chunk
    } deriving Show

opStateIntegerDefault :: OpStateInteger
opStateIntegerDefault = OpStateInteger { base = BaseDec, chunk = Chunk }

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

--intOps :: (Integral a, Bits a) => [(String, Engine a OpStateInteger -> Engine a OpStateInteger)]
intOps = Map.fromList $ numericOps ++
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

integerCalculator :: Stack Integer -> OpStateInteger -> IntegerCalculator
integerCalculator initialStack initialState = IntegerCalculator{ engine = (Engine initialStack initialState) }

instance CalculatorClass IntegerCalculator where
    calcDisplay c = displayInteger (engine c)

    -- Special case: if it's 0x or 0X it could be the beginning of
    -- a valid hex number so let it ride.
    calcConsume calc "0x" = (calc, "0x")
    calcConsume calc "0X" = (calc, "0X")
    -- Allow _ to stand in for prefix negation
    calcConsume calc "_"  = (calc, "-")
    calcConsume calc str = 
        case reads str of
            -- Special case: if it's a valid number with nothing or a dot
            -- following it could continue on to be a bigger number, so
            -- continue accumulating characters.
            (_, ""):_   -> (calc, str)
            (_, "."):_  -> (calc, str)
            (num, rest):_ ->
                -- special case: if the character caused a token to be returned, it might
                -- be the case that the remainder is also a valid token (consider the input
                -- "123+", the + causes the number to be completed as a token but it is
                -- itself a token.
                let newCalc = calc{engine=push (engine calc) num}
                in calcConsume newCalc rest
            []            -> case Map.lookup str intOps of
                Just f -> (calc{engine=f (engine calc)}, "")
                Nothing -> (calc, str)

newIntegerCalculator :: IntegerCalculator
newIntegerCalculator = integerCalculator [0,0,0,0] opStateIntegerDefault
