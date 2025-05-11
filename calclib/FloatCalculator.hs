module FloatCalculator ( FloatCalculator(FloatCalculator), opStateFloatDefault ) where

import Calculator
import qualified Data.Map.Strict as Map
import Text.Printf

-- OpStateFloat is the operational state for the FloatCalculator
--   - prec: precision (number of places after the decimal)
--   - width: align numbers on the right side, with the given field width
data OpStateFloat = OpStateFloat
    { prec :: Maybe Int
    , width :: Maybe Int
    } deriving Show

-- opStateFloatDefault is the operational state that a FloatCalculator
-- should start with.
opStateFloatDefault :: OpStateFloat
opStateFloatDefault = OpStateFloat{ prec = Nothing, width = Nothing }

-- setp sets the precision
setp :: Engine Float OpStateFloat -> Engine Float OpStateFloat
setp (Engine (x:xs) ops) = Engine (ensureStack xs) ops{prec=Just $ floor x}
setp _ = error("setp underflow")

-- setw sets the width
setw :: Engine Float OpStateFloat -> Engine Float OpStateFloat
setw (Engine (x:xs) ops) = Engine (ensureStack xs) ops{width=Just $ floor x}
setw _ = error("setw underflow")

-- floatOps is a list of all of the operations available in the FloatCalculator.
-- This includes the commom numericOps, mathematical operations that are specific
-- to Floats, and operations on the operational state.
floatOps :: Map.Map String (Engine Float OpStateFloat -> Engine Float OpStateFloat)
floatOps = Map.fromList $ numericOps ++
        [ ("/", stackOp2(/))
        , ("^", stackOp2(**))
        , ("clearp", opStateOp (\s -> s{prec = Nothing}))
        , ("setp", setp)
        , ("clearw", opStateOp (\s -> s{width = Nothing}))
        , ("setw", setw)
        ] 

-- displayFloat displays the state of a FloatCalculator
displayFloat :: Engine Float OpStateFloat -> IO ()
displayFloat (Engine (x:y:z:t:_) ops) = do
    let format = case ((width ops), (prec ops)) of
            (Nothing, Nothing) -> "%g"
            (Just w, Nothing) -> printf "%%%dg" w
            (Nothing, Just p) -> printf "%%.%dg" p
            (Just w, Just p) -> printf "%%%d.%dg" w p
        widthstr = maybe "" (printf " w=%d") (width ops)
        precstr = maybe "" (printf " p=%d") (prec ops)
    printf "Float%s%s\n" widthstr precstr
    printf ("t " ++ format ++ "\n") t
    printf ("z " ++ format ++ "\n") z
    printf ("y " ++ format ++ "\n") y
    printf ("x " ++ format ++ "\n") x
displayFloat _ = error("displayFloat underflow")

-- FloatCalculator holds the state of a floating-point calculator.
data FloatCalculator = FloatCalculator (Engine Float OpStateFloat)

-- floatConsume attempts to find a prefix of the given string that represents a
-- datum or one of the defined floating-point operations, updates the engine if
-- one is found, and then returns the updated engine and the remainder of the
-- string.
floatConsume :: Engine Float OpStateFloat -> String -> (Engine Float OpStateFloat, String)
floatConsume = genericConsume (flip Map.lookup floatOps) reads

instance Calculator FloatCalculator where
    calcDisplay (FloatCalculator engine) = displayFloat engine
    calcConsume (FloatCalculator engine) str = let (eng, rest) = floatConsume engine str in (FloatCalculator(eng), rest)
