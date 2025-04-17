module Main where

import qualified Data.Map.Strict as Map

{- from unix library -}
import System.Posix.Terminal
import System.Posix.IO (fdRead, stdInput)

{- from base -}
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Control.Exception (finally, catch, IOException)

{- https://stackoverflow.com/questions/23068218/haskell-read-raw-keyboard-input
 - run an application in raw input / non-canonical mode with given
 - VMIN and VTIME settings. for a description of these, see:
 - http://www.gnu.org/software/libc/manual/html_node/Noncanonical-Input.html
 - as well as `man termios`.
 -}
withRawInput :: Int -> Int -> IO a -> IO a
withRawInput vmin vtime application = do

  {- retrieve current settings -}
  oldTermSettings <- getTerminalAttributes stdInput

  {- modify settings -}
  let newTermSettings =
        flip withoutMode  EnableEcho   . -- don't echo keystrokes
        flip withoutMode  ProcessInput . -- turn on non-canonical mode
        flip withTime     vtime        . -- wait at most vtime decisecs per read
        flip withMinInput vmin         $ -- wait for >= vmin bytes per read
        oldTermSettings

  {- install new settings -}
  setTerminalAttributes stdInput newTermSettings Immediately

  {- restore old settings no matter what; this prevents the terminal
   - from becoming borked if the application halts with an exception
   -}
  application
    `finally` setTerminalAttributes stdInput oldTermSettings Immediately


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

numericOps :: Num a => [(String, Engine a b -> Engine a b)]
numericOps =
        [ ("+", stackOp2(+))
        , ("-", stackOp2(-))
        , ("*", stackOp2(*))
        , ("neg", stackOp1 negate)
        , ("dup", dup)
        , ("swap", swap)
        , ("rot", rot)
        ]

--setp :: Engine Float OpStateFloat -> Engine Float OpStateFloat
--setp (Engine (Stack x y z t) opState) = Engine (Stack y z t t) opState{prec=x}

floatOps :: [(String, Engine Float OpStateFloat -> Engine Float OpStateFloat)]
floatOps = numericOps ++
        [ ("/", stackOp2(/))
        , ("clearp", opStateOp (\s -> s{prec = Nothing}))
        --, ("setp", setp)
        ] 

setBase :: Engine Integer OpStateInteger -> Engine Integer OpStateInteger
setBase engine@(Engine (Stack x y z t) opState) = case x of
    16 -> Engine (Stack y z t t) opState{base=BaseHex}
    10 -> Engine (Stack y z t t) opState{base=BaseDec}
    2 -> Engine (Stack y z t t) opState{base=BaseBin}
    _ -> engine

intOps :: [(String, Engine Integer OpStateInteger -> Engine Integer OpStateInteger)]
intOps = numericOps ++
        [ ("base", setBase)
        , ("/", stackOp2(div))
        , ("%", stackOp2(mod))
        ]

floatCalculator = Calculator
    { engine = Engine
        { stack = Stack 0 0 0 0
        , opState = OpStateFloat{ prec = Nothing }
        }
    , opsMap = Map.fromList floatOps
    , readNum = reads :: String -> [(Float,String)] 
    }

intCalculator = Calculator
    { engine = Engine
        { stack = Stack 0 0 0 0
        , opState = OpStateInteger { base = BaseDec }
        }
    , opsMap = Map.fromList intOps
    , readNum = reads :: String -> [(Integer,String)] 
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
parseChar calc [] '\DEL' = (Nothing, [])
parseChar calc acc '\DEL' = case reverse acc of x:xs -> (Nothing, reverse xs)
parseChar calc [] '\BS' = (Nothing, [])
parseChar calc acc '\BS' = case reverse acc of x:xs -> (Nothing, reverse xs)
parseChar calc acc c =
    let
        newacc = reverse (c:(reverse acc))
    in case tryParse calc newacc of
        -- special case: if we parsed the complete input as a number, don't return a token:
        -- it may be the case that the next character extends the number
        (Just (Num _), []) -> (Nothing, newacc)
        (t, r) -> (t, r)

showCalculator :: (Show a, Show b) => Calculator a b -> [Char] -> IO()
showCalculator calc acc = do
    putStr "\ESC[1J\ESC[H"
    print calc
    putStr ("> " ++ acc)

doCalculator :: (Show a, Show b, Read a) => Calculator a b -> [Char] -> [Char] -> IO ()
doCalculator initialCalc acc [] = return ()
doCalculator initialCalc acc (x:xs) =
    let (newCalc, newAcc) = case parseChar initialCalc acc x of
            (Nothing, rest) -> (initialCalc, rest)
            (Just t, rest) -> 
                -- special case: if the character caused a token to be returned, it might
                -- be the case that the remainder is also a valid token (consider the input
                -- "123+", the + causes the number to be completed as a token but it is
                -- itself a token.
                let
                    calc2 = consumeToken initialCalc t
                in
                    case tryParse calc2 rest of
                        (Nothing, rest) -> (calc2, rest)
                        (Just t, rest) -> (consumeToken calc2 t, rest)
    in case newAcc of 
        "_x" -> return ()
        "_f" -> startCalculator floatCalculator xs
        "_i" -> startCalculator intCalculator xs
        other -> do
            showCalculator newCalc newAcc
            doCalculator newCalc newAcc xs

startCalculator :: (Show a, Show b, Read a) => Calculator a b -> [Char] -> IO ()
startCalculator calc input = do
    showCalculator calc ""
    doCalculator calc "" input

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- getContents
    withRawInput 0 0 $ startCalculator floatCalculator input
