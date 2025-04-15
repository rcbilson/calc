module Main where

import qualified Data.Map.Strict as Map

data Stack a = Stack { x :: a, y :: a, z :: a, t :: a }
  deriving Show

stackOp1 :: (a -> a) -> Engine a b -> Engine a b
stackOp1 f (Engine (Stack x y z t) ops) = Engine (Stack (f x) y z t) ops
stackOp2 :: (a -> a -> a) -> Engine a b -> Engine a b
stackOp2 f (Engine (Stack x y z t) ops) = Engine (Stack (f y x) z t t) ops
opStateOp :: (b -> b) -> Engine a b -> Engine a b
opStateOp f (Engine stack ops) = Engine stack (f ops)

data Base = BaseDec | BaseHex | BaseBin deriving Show
data OpStateInteger = OpStateInteger
    { base :: Base
    } deriving Show
data OpStateFloat = OpStateFloat
    { prec :: Maybe Int
    } deriving Show
data Engine a b = Engine
    { stack :: Stack a
    , opState :: b
    } deriving Show
data Calculator a b = Calculator
    { engine :: Engine a b
    , opsMap :: Map.Map String (Engine a b -> Engine a b)
    , readNum :: String -> [(a,String)]
    }

instance (Show a, Show b) => Show (Calculator a b) where
    show calc = show (engine calc)

push :: Engine a b -> a -> Engine a b
push (Engine (Stack x y z t) ops) q = Engine (Stack q x y z) ops

dup :: Engine a b -> Engine a b
dup (Engine (Stack x y z t) ops) = Engine (Stack x x y z) ops

swap :: Engine a b -> Engine a b
swap (Engine (Stack x y z t) ops) = Engine (Stack y x y z) ops

rot :: Engine a b -> Engine a b
rot (Engine (Stack x y z t) ops) = Engine (Stack y z t x) ops

floatEngine = Engine
    { stack = Stack 0 0 0 0
    , opState = OpStateFloat{ prec = Nothing }
    }

floatCalculator = Calculator
    { engine = floatEngine
    , opsMap = Map.fromList
        [ ("+", stackOp2(+))
        , ("-", stackOp2(-))
        , ("*", stackOp2(*))
        , ("/", stackOp2(/))
        , ("neg", stackOp1 negate)
        , ("clearp", opStateOp (\s -> s{prec = Nothing}))
        , ("dup", dup)
        , ("swap", swap)
        , ("rot", rot)
        ]
    , readNum = reads :: String -> [(Float,String)] 
    }

data Token a = Op String | Num a deriving Show

consumeToken :: Calculator a b -> Token a -> Calculator a b
consumeToken calc token =
    case token of
        Op op -> case Map.lookup op (opsMap calc) of
            Just f -> calc{engine=f (engine calc)}
            Nothing -> error ("Unknown operator: " ++ op)
        Num n -> calc{engine=push (engine calc) n}

tryParse :: Calculator a b -> [Char] -> (Maybe (Token a), [Char])
tryParse calc [] = (Nothing, [])
tryParse calc str =
    case (readNum calc) str of
        (num, rest):_ -> (Just (Num num), rest)
        [] ->
            if Map.member str (opsMap calc) then (Just (Op str), [])
            else (Nothing, str)

parseChar :: Calculator a b -> [Char] -> Char -> (Maybe (Token a), [Char])
parseChar calc acc ' ' = case tryParse calc acc of (t, r) -> (t, [])
parseChar calc acc '\n' = case tryParse calc acc of (t, r) -> (t, [])
parseChar calc [] '\177' = (Nothing, [])
parseChar calc acc '\177' = case reverse acc of x:xs -> (Nothing, reverse xs)
parseChar calc [] '\008' = (Nothing, [])
parseChar calc acc '\008' = case reverse acc of x:xs -> (Nothing, reverse xs)
parseChar calc acc c =
    let
        newacc = reverse (c:(reverse acc))
    in case tryParse calc newacc of
        -- special case: if we parsed the complete input as a number, don't return a token:
        -- it may be the case that the next character extends the number
        (Just (Num _), []) -> (Nothing, newacc)
        (t, r) -> (t, r)

doCalculator :: (Show a, Show b, Read a) => Calculator a b -> [Char] -> [Char] -> IO ()
doCalculator initialCalc acc [] = return ()
doCalculator initialCalc acc (x:xs) =
    let (newCalc, newAcc) = case parseChar initialCalc acc x of
            (Nothing, rest) -> (initialCalc, rest)
            (Just t, rest) -> 
                let
                    -- special case: if the character caused a token to be returned, it might
                    -- be the case that the remainder is also a valid token (consider the input
                    -- "123+", the + causes the number to be completed as a token but it is
                    -- itself a token.
                    calc2 = consumeToken initialCalc t
                in
                    case tryParse calc2 rest of
                        (Nothing, rest) -> (calc2, rest)
                        (Just t, rest) -> (consumeToken calc2 t, rest)
    in do
        putStrLn ""
        print newCalc
        putStr ("> " ++ newAcc)
        doCalculator newCalc newAcc xs

main :: IO ()
main = do
    input <- getContents
    doCalculator floatCalculator "" input
