module ConvertibleCalculator ( ConvertibleCalculator, calcToInteger, calcToWord8 ) where

import Calculator
import IntegerCalculator

class ConvertibleCalculator a where
    calcToInteger :: a -> IntegerCalculator
    calcToWord8   :: a -> Word8Calculator

instance ConvertibleCalculator IntegerCalculator where
    calcToInteger = id
    calcToWord8 (IntegerCalculator (Engine stk ops)) = Word8Calculator (Engine (map fromIntegral stk) ops)

instance ConvertibleCalculator Word8Calculator where
    calcToInteger (Word8Calculator (Engine stk ops)) = IntegerCalculator (Engine (map fromIntegral stk) ops)
    calcToWord8 = id
