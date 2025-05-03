module Calculator ( Calculator, calcDisplay, calcConsume, Stack, ensureStack, stackOp1, stackOp2, opStateOp, Engine( Engine ), push, numericOps, genericConsume ) where

class Calculator a where
    calcDisplay :: a -> IO()
    calcConsume :: a -> String -> (a, String)

type Stack a = [a]

ensureStack :: Num a => Stack a -> Stack a
ensureStack stk
    | length(stk) < 4 = ensureStack (stk ++ [0])
    | otherwise       = stk

type EngineFn a b = Engine a b -> Engine a b

stackOp1 :: (a -> a) -> EngineFn a b
stackOp1 f (Engine (x:xs) ops) = Engine ((f x):xs) ops
stackOp1 _ _ = error("stackOp1 underflow")
stackOp2 :: Num a => (a -> a -> a) -> EngineFn a b
stackOp2 f (Engine (x:y:xs) ops) = Engine (ensureStack $ (f y x):xs) ops
stackOp2 _ _ = error("stackOp2 underflow")
opStateOp :: (b -> b) -> EngineFn a b
opStateOp f (Engine stk ops) = Engine stk (f ops)

data Engine a b = Engine
    { stack :: Stack a
    , opState :: b
    } deriving Show

push :: Engine a b -> a -> Engine a b
push (Engine stk ops) q = Engine (q:stk) ops

dup :: Engine a b -> Engine a b
dup (Engine (x:xs) ops) = Engine (x:x:xs) ops
dup _ = error("dup underflow")

swap :: Engine a b -> Engine a b
swap (Engine (x:y:xs) ops) = Engine (y:x:xs) ops
swap _ = error("swap underflow")

drop1 :: Engine a b -> Engine a b
drop1 (Engine (_:xs) ops) = Engine xs ops
drop1 _ = error("drop1 underflow")

numericOps :: Num a => [(String, Engine a b -> Engine a b)]
numericOps =
        [ ("+", stackOp2(+))
        , ("-", stackOp2(-))
        , ("*", stackOp2(*))
        , ("neg", stackOp1 negate)
        , ("dup", dup)
        , ("swap", swap)
        , ("drop", drop1)
        , (" ", id)
        , ("\n", id)
        ]

genericConsume :: (String -> Maybe (EngineFn a b)) -> (String -> [(a, String)]) -> Engine a b -> String -> (Engine a b, String)
-- Special case: if it's 0x or 0X it could be the beginning of
-- a valid hex number so let it ride.
genericConsume _ _ eng "0x" = (eng, "0x")
genericConsume _ _ eng "0X" = (eng, "0X")
-- Allow _ to stand in for prefix negation
genericConsume _ _ eng "_"  = (eng, "-")
genericConsume lookupOp readNum eng str = 
    case readNum str of
        -- Special case: if it's a valid number with nothing or a dot
        -- following it could continue on to be a bigger number, so
        -- continue accumulating characters.
        (_, ""):_   -> (eng, str)
        (_, "."):_  -> (eng, str)
        (num, rest):_ ->
            -- special case: if the character caused a token to be returned, it might
            -- be the case that the remainder is also a valid token (consider the input
            -- "123+", the + causes the number to be completed as a token but it is
            -- itself a token.
            let newEng = push eng num
            in genericConsume lookupOp readNum newEng rest
        [] -> case lookupOp str of
            Just f -> (f eng, "")
            Nothing -> (eng, str)
