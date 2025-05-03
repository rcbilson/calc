module FloatCalculator ( FloatCalculator(FloatCalculator), opStateFloatDefault ) where

import Calculator
import qualified Data.Map.Strict as Map
import Text.Printf

data OpStateFloat = OpStateFloat
    { prec :: Maybe Int
    , width :: Maybe Int
    } deriving Show

opStateFloatDefault :: OpStateFloat
opStateFloatDefault = OpStateFloat{ prec = Nothing, width = Nothing }

setp :: Engine Float OpStateFloat -> Engine Float OpStateFloat
setp (Engine (x:xs) ops) = Engine (ensureStack xs) ops{prec=Just $ floor x}
setp _ = error("setp underflow")

setw :: Engine Float OpStateFloat -> Engine Float OpStateFloat
setw (Engine (x:xs) ops) = Engine (ensureStack xs) ops{width=Just $ floor x}
setw _ = error("setw underflow")

floatOps :: Map.Map String (Engine Float OpStateFloat -> Engine Float OpStateFloat)
floatOps = Map.fromList $ numericOps ++
        [ ("/", stackOp2(/))
        , ("^", stackOp2(**))
        , ("clearp", opStateOp (\s -> s{prec = Nothing}))
        , ("setp", setp)
        , ("clearw", opStateOp (\s -> s{width = Nothing}))
        , ("setw", setw)
        ] 

displayFloat :: Engine Float OpStateFloat -> IO ()
displayFloat (Engine (x:y:z:t:_) ops) = do
    let format = case ((width ops), (prec ops)) of
            (Nothing, Nothing) -> "%g"
            (Just w, Nothing) -> printf "%%%dg" w
            (Nothing, Just p) -> printf "%%.%dg" p
            (Just w, Just p) -> printf "%%%d.%dg" w p
    putStrLn $ show ops
    printf ("t " ++ format ++ "\n") t
    printf ("z " ++ format ++ "\n") z
    printf ("y " ++ format ++ "\n") y
    printf ("x " ++ format ++ "\n") x
displayFloat _ = error("displayFloat underflow")

data FloatCalculator = FloatCalculator (Engine Float OpStateFloat)

instance Calculator FloatCalculator where
    calcDisplay (FloatCalculator engine) = displayFloat engine
    calcConsume (FloatCalculator engine) str = let (eng, rest) = genericConsume (flip Map.lookup floatOps) reads engine str in (FloatCalculator(eng), rest)
