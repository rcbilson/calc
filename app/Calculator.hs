module Calculator ( Calculator, calcDisplay, calcConsume, Stack, ensureStack, stackOp1, stackOp2, opStateOp, Engine( Engine ), push, numericOps ) where
class Calculator a where
    calcDisplay :: a -> IO()
    calcConsume :: a -> String -> (a, String)

type Stack a = [a]

ensureStack :: Num a => Stack a -> Stack a
ensureStack stk
    | length(stk) < 4 = ensureStack (stk ++ [0])
    | otherwise       = stk

stackOp1 :: (a -> a) -> Engine a b -> Engine a b
stackOp1 f (Engine (x:xs) ops) = Engine ((f x):xs) ops
stackOp1 _ _ = error("stackOp1 underflow")
stackOp2 :: Num a => (a -> a -> a) -> Engine a b -> Engine a b
stackOp2 f (Engine (x:y:xs) ops) = Engine (ensureStack $ (f y x):xs) ops
stackOp2 _ _ = error("stackOp2 underflow")
opStateOp :: (b -> b) -> Engine a b -> Engine a b
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
