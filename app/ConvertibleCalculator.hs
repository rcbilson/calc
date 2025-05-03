module ConvertibleCalculator ( ConvertibleCalculator, calcToInteger, calcToWord8, calcToWord16, calcToWord32, calcToWord64 ) where

import Calculator
import IntegerCalculator

class ConvertibleCalculator a where
    calcToInteger :: a -> IntegerCalculator
    calcToWord8   :: a -> Word8Calculator
    calcToWord16   :: a -> Word16Calculator
    calcToWord32   :: a -> Word32Calculator
    calcToWord64   :: a -> Word64Calculator

instance ConvertibleCalculator IntegerCalculator where
    calcToInteger = id
    calcToWord8 (IntegerCalculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)
    calcToWord16 (IntegerCalculator (Engine stk ops)) = Word16Calculator (Engine (map fromIntegral stk) ops)
    calcToWord32 (IntegerCalculator (Engine stk ops)) = Word32Calculator (Engine (map fromIntegral stk) ops)
    calcToWord64 (IntegerCalculator (Engine stk ops)) = Word64Calculator (Engine (map fromIntegral stk) ops)

instance ConvertibleCalculator Word8Calculator where
    calcToInteger (Word8Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8 = id
    calcToWord16 (Word8Calculator (Engine stk ops)) = Word16Calculator (Engine (map fromIntegral stk) ops)
    calcToWord32 (Word8Calculator (Engine stk ops)) = Word32Calculator (Engine (map fromIntegral stk) ops)
    calcToWord64 (Word8Calculator (Engine stk ops)) = Word64Calculator (Engine (map fromIntegral stk) ops)

instance ConvertibleCalculator Word16Calculator where
    calcToInteger (Word16Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8 (Word16Calculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)
    calcToWord16 = id
    calcToWord32 (Word16Calculator (Engine stk ops)) = Word32Calculator (Engine (map fromIntegral stk) ops)
    calcToWord64 (Word16Calculator (Engine stk ops)) = Word64Calculator (Engine (map fromIntegral stk) ops)

instance ConvertibleCalculator Word32Calculator where
    calcToInteger (Word32Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8 (Word32Calculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)
    calcToWord16 (Word32Calculator (Engine stk ops)) = Word16Calculator (Engine (map fromIntegral stk) ops)
    calcToWord32 = id
    calcToWord64 (Word32Calculator (Engine stk ops)) = Word64Calculator (Engine (map fromIntegral stk) ops)

instance ConvertibleCalculator Word64Calculator where
    calcToInteger (Word64Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8 (Word64Calculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)
    calcToWord16 (Word64Calculator (Engine stk ops)) = Word16Calculator (Engine (map fromIntegral stk) ops)
    calcToWord32 (Word64Calculator (Engine stk ops)) = Word32Calculator (Engine (map fromIntegral stk) ops)
    calcToWord64 = id
