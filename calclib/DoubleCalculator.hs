module DoubleCalculator (
    testDoubleCalculator,
    newDoubleCalculator,
    defaultCalculator,
    OpStateDouble,
    opStateDoubleDefault) where

import Calculator
import qualified Data.Map.Strict as Map
import Data.Array
import Text.Printf
import Text.Regex.TDFA

-- OpStateDouble is the operational state for the DoubleCalculator
--   - prec: precision (number of places after the decimal)
--   - width: align numbers on the right side, with the given field width
data OpStateDouble = OpStateDouble
    { prec :: Maybe Int
    , width :: Maybe Int
    , dms :: Bool
    } deriving Show

-- opStateDoubleDefault is the operational state that a DoubleCalculator
-- should start with.
opStateDoubleDefault :: OpStateDouble
opStateDoubleDefault = OpStateDouble{ prec = Nothing, width = Nothing, dms = False }

-- setp sets the precision
setp :: EngineFn Double OpStateDouble
setp (Engine (x:xs) ops) = (Engine (ensureStack xs) ops{prec=Just $ floor x}, Undo [])
setp _ = error("setp underflow")

-- setw sets the width
setw :: EngineFn Double OpStateDouble
setw (Engine (x:xs) ops) = (Engine (ensureStack xs) ops{width=Just $ floor x}, Undo [])
setw _ = error("setw underflow")

-- floatOps is a list of all of the operations available in the DoubleCalculator.
-- This includes the commom numericOps, mathematical operations that are specific
-- to Doubles, and operations on the operational state.
floatOps :: Map.Map String (EngineFn Double OpStateDouble)
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
        , ("dms", opStateOp (\s -> s{dms = (not (dms s))}))
        ] 

-- doubleFormat constructs a printf format for Doubles
--   char must be 'e' 'f' 'g' per printf rules
doubleFormat :: String -> OpStateDouble -> String
doubleFormat char ops =
    case ((width ops), (prec ops)) of
        (Nothing, Nothing) -> "%" ++ char
        (Just w, Nothing) -> printf ("%%%d" ++ char) w
        (Nothing, Just p) -> printf ("%%.%d" ++ char) p
        (Just w, Just p) -> printf ("%%%d.%d" ++ char) w p

-- displayOpState formats an OpStateDouble for display
displayOpState :: OpStateDouble -> IO ()
displayOpState ops =
    let widthstr = maybe "" (printf " w=%d") (width ops)
        precstr = maybe "" (printf " p=%d") (prec ops)
        dmsstr = if dms ops then " dms" else ""
    in printf "Double%s%s%s\n" widthstr precstr dmsstr

-- displayDouble displays the state of a DoubleCalculator
displayDoubleOrdinary :: Engine Double OpStateDouble -> IO ()
displayDoubleOrdinary (Engine (x:y:z:t:_) ops) = do
    let format = doubleFormat "g" ops
    displayOpState ops
    printf ("t " ++ format ++ "\n") t
    printf ("z " ++ format ++ "\n") z
    printf ("y " ++ format ++ "\n") y
    printf ("x " ++ format ++ "\n") x
displayDoubleOrdinary _ = error("displayDouble underflow")

-- displayDMS formats a Double in D/M/S format
displayDMS :: OpStateDouble -> Double -> String
displayDMS ops x =
    let deg = floor x
        min_f = (x - (fromInteger deg)) * 60
        min = floor min_f
        sec_f = (min_f - (fromInteger min)) * 60
        sec = floor sec_f
        frac = sec_f - (fromInteger sec)
        fracstr = printf (doubleFormat "f" ops) frac
    in printf ("%02d:%02d:%02d%s") deg min sec (drop 1 (fracstr :: String))

-- displayDouble displays the state of a DoubleCalculator in D/M/S format
displayDoubleDMS :: Engine Double OpStateDouble -> IO ()
displayDoubleDMS (Engine (x:y:z:t:_) ops) = do
    displayOpState ops
    putStrLn ("t " ++ (displayDMS ops t))
    putStrLn ("z " ++ (displayDMS ops z))
    putStrLn ("y " ++ (displayDMS ops y))
    putStrLn ("x " ++ (displayDMS ops x))
displayDoubleDMS _ = error("displayDouble underflow")

-- displayDouble displays the state of a DoubleCalculator in D/M/S format
displayDouble :: Engine Double OpStateDouble -> IO ()
displayDouble eng@(Engine _ ops) =
    if dms ops
    then displayDoubleDMS eng
    else displayDoubleOrdinary eng

-- dmsRegex describes a D/M/S constant
dmsRegex :: Regex
dmsRegex = makeRegex "^([0-9]+):([0-9]+)(:([0-9]+))?"

-- readDouble parses a double value from the given string
--   it accepts either D/M/S or 'reads' format
--   it returns an empty list if nothing was parsed
--   otherwise it returns a list with a pair of the Double and the remainder of the string
readDouble :: String -> [(Double, String)]
readDouble inp =
    case matchOnceText dmsRegex inp of
        Nothing -> reads inp
        Just (_, arr, rest) ->
            case elems arr of
                (_:(d,_):(m,_):_:("",_):_) -> assemble (read d) (read m) 0 rest
                (_:(d,_):(m,_):_:(s,_):_) -> assemble (read d) (read m) (read s) rest
    where
        assemble d m s rest = [(d + (m/60) + (s/3600), rest)]

newDoubleCalculator :: Stack Double -> OpStateDouble -> Calculator Double OpStateDouble
newDoubleCalculator stk ops = Calculator
    { calcEngine = Engine stk ops
    , calcUndos = []
    , calcRedos = []
    , calcOp = flip Map.lookup floatOps
    , calcReads = readDouble
    , calcDisp = displayDouble . calcEngine
    }

testDoubleCalculator :: Calculator Double OpStateDouble
testDoubleCalculator = newDoubleCalculator [0,0,0,0] opStateDoubleDefault

-- defaultCalculator is the kind of calculator used when the program starts.
defaultCalculator :: Calculator Double OpStateDouble
defaultCalculator = testDoubleCalculator
