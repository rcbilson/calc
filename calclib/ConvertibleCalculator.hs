module ConvertibleCalculator (
    ConvertibleCalculator,
    calcToInteger,
    calcToWord8,
    calcToWord16,
    calcToWord32,
    calcToWord64,
    calcToDouble ) where

import Calculator
import IntegerCalculator
import DoubleCalculator

-- A ConvertibleCalculator is a Calculator that can be converted to one of the
-- other calculator types.
--
-- This is pretty gnarly because it requires a matrix of source and destination
-- state types. It feels like it ought to be possible to abstract the
-- implementation more; I've tried a few things and didn't come up with
-- anything that was notably less gnarly. This version has the virtue of being
-- obvious.
class Calculator a => ConvertibleCalculator a where
    calcToInteger :: a -> IntegerCalculator
    calcToWord8   :: a -> Word8Calculator
    calcToWord16  :: a -> Word16Calculator
    calcToWord32  :: a -> Word32Calculator
    calcToWord64  :: a -> Word64Calculator
    calcToDouble   :: a -> DoubleCalculator

instance ConvertibleCalculator IntegerCalculator where
    calcToInteger = id
    calcToWord8  (IntegerCalculator (Engine stk ops) _ _) = makeWord8Calculator (map fromIntegral stk)
    calcToWord16 (IntegerCalculator (Engine stk ops) _ _) = makeWord16Calculator (map fromIntegral stk)
    calcToWord32 (IntegerCalculator (Engine stk ops) _ _) = makeWord32Calculator (map fromIntegral stk)
    calcToWord64 (IntegerCalculator (Engine stk ops) _ _) = makeWord64Calculator (map fromIntegral stk)
    calcToDouble (IntegerCalculator (Engine stk _) _ _)  = makeDoubleCalculator stk

instance ConvertibleCalculator Word8Calculator where
    calcToInteger (Word8Calculator (Engine stk ops) _ _) = makeIntegerCalculator (map fromIntegral stk)
    calcToWord8   = id
    calcToWord16  (Word8Calculator (Engine stk ops) _ _) = makeWord16Calculator (map fromIntegral stk)
    calcToWord32  (Word8Calculator (Engine stk ops) _ _) = makeWord32Calculator (map fromIntegral stk)
    calcToWord64  (Word8Calculator (Engine stk ops) _ _) = makeWord64Calculator (map fromIntegral stk)
    calcToDouble  (Word8Calculator (Engine stk _) _ _)   = makeDoubleCalculator stk

instance ConvertibleCalculator Word16Calculator where
    calcToInteger (Word16Calculator (Engine stk ops) _ _) = makeIntegerCalculator (map fromIntegral stk)
    calcToWord8   (Word16Calculator (Engine stk ops) _ _) = makeWord8Calculator (map fromIntegral stk)
    calcToWord16  = id
    calcToWord32  (Word16Calculator (Engine stk ops) _ _) = makeWord32Calculator (map fromIntegral stk)
    calcToWord64  (Word16Calculator (Engine stk ops) _ _) = makeWord64Calculator (map fromIntegral stk)
    calcToDouble  (Word16Calculator (Engine stk _) _ _)   = makeDoubleCalculator stk

instance ConvertibleCalculator Word32Calculator where
    calcToInteger (Word32Calculator (Engine stk ops) _ _) = makeIntegerCalculator (map fromIntegral stk)
    calcToWord8   (Word32Calculator (Engine stk ops) _ _) = makeWord8Calculator (map fromIntegral stk)
    calcToWord16  (Word32Calculator (Engine stk ops) _ _) = makeWord16Calculator (map fromIntegral stk)
    calcToWord32  = id
    calcToWord64  (Word32Calculator (Engine stk ops) _ _) = makeWord64Calculator (map fromIntegral stk)
    calcToDouble  (Word32Calculator (Engine stk _) _ _)   = makeDoubleCalculator stk

instance ConvertibleCalculator Word64Calculator where
    calcToInteger (Word64Calculator (Engine stk ops) _ _) = makeIntegerCalculator (map fromIntegral stk)
    calcToWord8   (Word64Calculator (Engine stk ops) _ _) = makeWord8Calculator (map fromIntegral stk)
    calcToWord16  (Word64Calculator (Engine stk ops) _ _) = makeWord16Calculator (map fromIntegral stk)
    calcToWord32  (Word64Calculator (Engine stk ops) _ _) = makeWord32Calculator (map fromIntegral stk)
    calcToWord64  = id
    calcToDouble  (Word64Calculator (Engine stk _) _ _)   = makeDoubleCalculator stk

instance ConvertibleCalculator DoubleCalculator where
    calcToInteger (DoubleCalculator (Engine stk _) _ _) = makeIntegerCalculator (map floor stk)
    calcToWord8   (DoubleCalculator (Engine stk _) _ _) = makeWord8Calculator (map floor stk)
    calcToWord16  (DoubleCalculator (Engine stk _) _ _) = makeWord16Calculator (map floor stk)
    calcToWord32  (DoubleCalculator (Engine stk _) _ _) = makeWord32Calculator (map floor stk)
    calcToWord64  (DoubleCalculator (Engine stk _) _ _) = makeWord64Calculator (map floor stk)
    calcToDouble   = id
