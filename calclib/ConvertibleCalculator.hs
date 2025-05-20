module ConvertibleCalculator (
    Convertible,
    OpState,
    calcToInteger,
    calcToWord8,
    calcToWord16,
    calcToWord32,
    calcToWord64,
    calcToDouble ) where

import Calculator
import IntegerCalculator
import DoubleCalculator
import Data.Word

class Convertible a where
    toIntger :: a -> Integer
    toWord8 :: a -> Word8
    toWord16 :: a -> Word16
    toWord32 :: a -> Word32
    toWord64 :: a -> Word64
    toDouble :: a -> Double

instance Convertible Integer where
    toIntger = id
    toWord8 x = fromIntegral x
    toWord16 x = fromIntegral x
    toWord32 x = fromIntegral x
    toWord64 x = fromIntegral x
    toDouble x = fromIntegral x

instance Convertible Word8 where
    toIntger x = fromIntegral x
    toWord8 = id
    toWord16 x = fromIntegral x
    toWord32 x = fromIntegral x
    toWord64 x = fromIntegral x
    toDouble x = fromIntegral x

instance Convertible Word16 where
    toIntger x = fromIntegral x
    toWord8 x = fromIntegral x
    toWord16 = id
    toWord32 x = fromIntegral x
    toWord64 x = fromIntegral x
    toDouble x = fromIntegral x

instance Convertible Word32 where
    toIntger x = fromIntegral x
    toWord8 x = fromIntegral x
    toWord16 x = fromIntegral x
    toWord32 = id
    toWord64 x = fromIntegral x
    toDouble x = fromIntegral x

instance Convertible Word64 where
    toIntger x = fromIntegral x
    toWord8 x = fromIntegral x
    toWord16 x = fromIntegral x
    toWord32 x = fromIntegral x
    toWord64 = id
    toDouble x = fromIntegral x

instance Convertible Double where
    toIntger x = floor x
    toWord8 x = floor x
    toWord16 x = floor x
    toWord32 x = floor x
    toWord64 x = floor x
    toDouble = id

class OpState a where
    toOpStateInteger :: a -> OpStateInteger
    toOpStateDouble :: a -> OpStateDouble

instance OpState OpStateInteger where
    toOpStateInteger = id
    toOpStateDouble _ = opStateDoubleDefault

instance OpState OpStateDouble where
    toOpStateInteger _ = opStateIntegerDefault
    toOpStateDouble = id

convertCalculator ctor a b c = case calcEngine c of Engine stk ops -> ctor (map a stk) (b ops)

calcToInteger :: (Convertible a, OpState b) => Calculator a b -> Calculator Integer OpStateInteger
calcToInteger = convertCalculator newIntegerCalculator toIntger toOpStateInteger

calcToWord8 :: (Convertible a, OpState b) => Calculator a b -> Calculator Word8 OpStateInteger
calcToWord8 = convertCalculator newWord8Calculator toWord8 toOpStateInteger

calcToWord16 :: (Convertible a, OpState b) => Calculator a b -> Calculator Word16 OpStateInteger
calcToWord16 = convertCalculator newWord16Calculator toWord16 toOpStateInteger

calcToWord32 :: (Convertible a, OpState b) => Calculator a b -> Calculator Word32 OpStateInteger
calcToWord32 = convertCalculator newWord32Calculator toWord32 toOpStateInteger

calcToWord64 :: (Convertible a, OpState b) => Calculator a b -> Calculator Word64 OpStateInteger
calcToWord64 = convertCalculator newWord64Calculator toWord64 toOpStateInteger

calcToDouble :: (Convertible a, OpState b) => Calculator a b -> Calculator Double OpStateDouble
calcToDouble = convertCalculator newDoubleCalculator toDouble toOpStateDouble
