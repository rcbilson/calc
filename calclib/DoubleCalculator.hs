module DoubleCalculator ( DoubleCalculator(DoubleCalculator), opStateDoubleDefault ) where

import Calculator
import qualified Data.Map.Strict as Map
import Text.Printf

-- OpStateDouble is the operational state for the DoubleCalculator
--   - prec: precision (number of places after the decimal)
--   - width: align numbers on the right side, with the given field width
data OpStateDouble = OpStateDouble
    { prec :: Maybe Int
    , width :: Maybe Int
    } deriving Show

-- opStateDoubleDefault is the operational state that a DoubleCalculator
-- should start with.
opStateDoubleDefault :: OpStateDouble
opStateDoubleDefault = OpStateDouble{ prec = Nothing, width = Nothing }

-- setp sets the precision
setp :: Engine Double OpStateDouble -> Engine Double OpStateDouble
setp (Engine (x:xs) ops) = Engine (ensureStack xs) ops{prec=Just $ floor x}
setp _ = error("setp underflow")

-- setw sets the width
setw :: Engine Double OpStateDouble -> Engine Double OpStateDouble
setw (Engine (x:xs) ops) = Engine (ensureStack xs) ops{width=Just $ floor x}
setw _ = error("setw underflow")

-- floatOps is a list of all of the operations available in the DoubleCalculator.
-- This includes the commom numericOps, mathematical operations that are specific
-- to Doubles, and operations on the operational state.
floatOps :: Map.Map String (Engine Double OpStateDouble -> Engine Double OpStateDouble)
floatOps = Map.fromList $ numericOps ++
        [ ("/", stackOp2(/))
        , ("^", stackOp2(**))
        , ("log", stackOp2 (flip logBase))
        , ("ln", stackOp1 log)
        , ("lg", stackOp1 (logBase 2))
        , ("exp", stackOp1 exp)
        , ("pi", push pi)
        , ("sin", stackOp1 sin)
        , ("cos", stackOp1 cos)
        , ("tan", stackOp1 tan)
        , ("asin", stackOp1 asin)
        , ("acos", stackOp1 acos)
        , ("atan", stackOp1 atan)
        , ("clearp", opStateOp (\s -> s{prec = Nothing}))
        , ("setp", setp)
        , ("clearw", opStateOp (\s -> s{width = Nothing}))
        , ("setw", setw)
        ] 

-- displayDouble displays the state of a DoubleCalculator
displayDouble :: Engine Double OpStateDouble -> IO ()
displayDouble (Engine (x:y:z:t:_) ops) = do
    let format = case ((width ops), (prec ops)) of
            (Nothing, Nothing) -> "%g"
            (Just w, Nothing) -> printf "%%%dg" w
            (Nothing, Just p) -> printf "%%.%dg" p
            (Just w, Just p) -> printf "%%%d.%dg" w p
        widthstr = maybe "" (printf " w=%d") (width ops)
        precstr = maybe "" (printf " p=%d") (prec ops)
    printf "Double%s%s\n" widthstr precstr
    printf ("t " ++ format ++ "\n") t
    printf ("z " ++ format ++ "\n") z
    printf ("y " ++ format ++ "\n") y
    printf ("x " ++ format ++ "\n") x
displayDouble _ = error("displayDouble underflow")

-- DoubleCalculator holds the state of a floating-point calculator.
data DoubleCalculator = DoubleCalculator (Engine Double OpStateDouble)

-- floatConsume attempts to find a prefix of the given string that represents a
-- datum or one of the defined floating-point operations, updates the engine if
-- one is found, and then returns the updated engine and the remainder of the
-- string.
floatConsume :: Engine Double OpStateDouble -> String -> (Engine Double OpStateDouble, String)
floatConsume = genericConsume (flip Map.lookup floatOps) reads

instance Calculator DoubleCalculator where
    calcDisplay (DoubleCalculator engine) = displayDouble engine
    calcConsume (DoubleCalculator engine) str = let (eng, rest) = floatConsume engine str in (DoubleCalculator(eng), rest)
