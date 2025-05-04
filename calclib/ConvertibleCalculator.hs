module ConvertibleCalculator (
    ConvertibleCalculator,
    calcToInteger,
    calcToWord8,
    calcToWord16,
    calcToWord32,
    calcToWord64,
    calcToFloat ) where

import Calculator
import IntegerCalculator
import FloatCalculator

-- A ConvertibleCalculator is a Calculator that can be converted to one of the
-- other calculator types.
--
-- This is pretty gnarly because it requires a matrix of source and destination
-- state types. It feels like it ought to be possible to abstract the
-- implementation more; I've tried a few things and didn't come up with
-- anything that was notably less gnarly. This version has the virtue of being
-- obvious.
class ConvertibleCalculator a where
    calcToInteger :: a -> IntegerCalculator
    calcToWord8   :: a -> Word8Calculator
    calcToWord16  :: a -> Word16Calculator
    calcToWord32  :: a -> Word32Calculator
    calcToWord64  :: a -> Word64Calculator
    calcToFloat   :: a -> FloatCalculator

instance ConvertibleCalculator IntegerCalculator where
    calcToInteger = id
    calcToWord8  (IntegerCalculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)
    calcToWord16 (IntegerCalculator (Engine stk ops)) = Word16Calculator (Engine (map fromIntegral stk) ops)
    calcToWord32 (IntegerCalculator (Engine stk ops)) = Word32Calculator (Engine (map fromIntegral stk) ops)
    calcToWord64 (IntegerCalculator (Engine stk ops)) = Word64Calculator (Engine (map fromIntegral stk) ops)
    calcToFloat  (IntegerCalculator (Engine stk _))   = FloatCalculator (Engine (map fromIntegral stk) opStateFloatDefault)

instance ConvertibleCalculator Word8Calculator where
    calcToInteger (Word8Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8   = id
    calcToWord16  (Word8Calculator (Engine stk ops)) = Word16Calculator (Engine (map fromIntegral stk) ops)
    calcToWord32  (Word8Calculator (Engine stk ops)) = Word32Calculator (Engine (map fromIntegral stk) ops)
    calcToWord64  (Word8Calculator (Engine stk ops)) = Word64Calculator (Engine (map fromIntegral stk) ops)
    calcToFloat   (Word8Calculator (Engine stk _))   = FloatCalculator (Engine (map fromIntegral stk) opStateFloatDefault)

instance ConvertibleCalculator Word16Calculator where
    calcToInteger (Word16Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8   (Word16Calculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)
    calcToWord16  = id
    calcToWord32  (Word16Calculator (Engine stk ops)) = Word32Calculator (Engine (map fromIntegral stk) ops)
    calcToWord64  (Word16Calculator (Engine stk ops)) = Word64Calculator (Engine (map fromIntegral stk) ops)
    calcToFloat   (Word16Calculator (Engine stk _))   = FloatCalculator (Engine (map fromIntegral stk) opStateFloatDefault)

instance ConvertibleCalculator Word32Calculator where
    calcToInteger (Word32Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8   (Word32Calculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)
    calcToWord16  (Word32Calculator (Engine stk ops)) = Word16Calculator (Engine (map fromIntegral stk) ops)
    calcToWord32  = id
    calcToWord64  (Word32Calculator (Engine stk ops)) = Word64Calculator (Engine (map fromIntegral stk) ops)
    calcToFloat   (Word32Calculator (Engine stk _))   = FloatCalculator (Engine (map fromIntegral stk) opStateFloatDefault)

instance ConvertibleCalculator Word64Calculator where
    calcToInteger (Word64Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8   (Word64Calculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)
    calcToWord16  (Word64Calculator (Engine stk ops)) = Word16Calculator (Engine (map fromIntegral stk) ops)
    calcToWord32  (Word64Calculator (Engine stk ops)) = Word32Calculator (Engine (map fromIntegral stk) ops)
    calcToWord64  = id
    calcToFloat   (Word64Calculator (Engine stk _))   = FloatCalculator (Engine (map fromIntegral stk) opStateFloatDefault)

instance ConvertibleCalculator FloatCalculator where
    calcToInteger (FloatCalculator (Engine stk _)) = IntegerCalculator (Engine (map floor stk) opStateIntegerDefault)
    calcToWord8   (FloatCalculator (Engine stk _)) = Word8Calculator (Engine (map floor stk) opStateIntegerDefault)
    calcToWord16  (FloatCalculator (Engine stk _)) = Word16Calculator (Engine (map floor stk) opStateIntegerDefault)
    calcToWord32  (FloatCalculator (Engine stk _)) = Word32Calculator (Engine (map floor stk) opStateIntegerDefault)
    calcToWord64  (FloatCalculator (Engine stk _)) = Word64Calculator (Engine (map floor stk) opStateIntegerDefault)
    calcToFloat   = id
