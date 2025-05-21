module Calculator (
    Calculator( Calculator ),
    calcEngine,
    calcUndos,
    calcRedos,
    calcReads,
    calcOp,
    calcDisp,
    calcDisplay,
    calcApply,
    calcUndo,
    calcRedo,
    Stack,
    ensureStack,
    stackOp1,
    stackOp2,
    opStateOp,
    push,
    Engine( Engine ),
    EngineFn,
    Undo( Undo, NoUndo ),
    numericOps) where

-- The rest of this file are utility classes that are common to the different
-- Calculator implementations.

-- A stack is represented as a list, with the head of the list as top of stack
type Stack a = [a]

-- ensureStack takes a stack and produces an equivalent stack that is at least
-- four elements deep, filling in with zeroes at the base of the stack as
-- necessary.
ensureStack :: Num a => Stack a -> Stack a
ensureStack stk = take 4 (stk ++ repeat 0)

-- An Engine is the guts of a Calculator, consisting of a stack and some amount
-- of "operational state" that determines the behaviour of the calculator.
data Engine a b = Engine
    { stack :: Stack a
    , opState :: b
    } deriving Show

-- An EngineFn is any function that operates on an Engine, returning an Engine
-- of the same type.
type EngineFn a b = Engine a b -> (Engine a b, Undo a b)

-- An Undo is a list of EngineFn that can be run in order to reverse the action
-- of a previous EngineFn
data Undo a b = NoUndo | Undo [EngineFn a b]

-- stackOp1 turns a unary function into an EngineFn that applies the function
-- to the top element of the stack.
stackOp1 :: (a -> a) -> EngineFn a b
stackOp1 f (Engine (x:xs) ops) = (Engine ((f x):xs) ops, Undo [drop1, push x])
stackOp1 _ _ = error("stackOp1 underflow")

-- stackOp2 turns a binary function into an EngineFn that applies the function
-- to the top two elements of the stack.
stackOp2 :: Num a => (a -> a -> a) -> EngineFn a b
stackOp2 f (Engine (x:y:xs) ops) = (Engine (ensureStack $ (f y x):xs) ops, Undo [drop1, push y, push x])
stackOp2 _ _ = error("stackOp2 underflow")

-- opStateOp turns a unary function into an EngineFn that applies the function
-- to the operational state.
opStateOp :: (b -> b) -> EngineFn a b
opStateOp f (Engine stk ops) = (Engine stk (f ops), NoUndo)

-- push inserts the datum 'q' at the top of the stack.
push :: a -> EngineFn a b
push q (Engine stk ops) = (Engine (q:stk) ops, Undo [drop1])

-- dup duplicates the top item of the stack.
dup :: EngineFn a b
dup (Engine (x:xs) ops) = (Engine (x:x:xs) ops, Undo [drop1])
dup _ = error("dup underflow")

-- swap reverses the order of the top two stack items.
swap :: EngineFn a b
swap (Engine (x:y:xs) ops) = (Engine (y:x:xs) ops, Undo [swap])
swap _ = error("swap underflow")

-- drop1 discards the top item of the stack.
drop1 :: EngineFn a b
drop1 (Engine (x:xs) ops) = (Engine xs ops, Undo [push x])
drop1 _ = error("drop1 underflow")

-- numericOps is a list of pairs of name and EngineFn. These
-- functions can apply to any numeric type so they are available
-- in all of the calculator implementations.
numericOps :: Num a => [(String, EngineFn a b)]
numericOps =
        [ ("+", stackOp2(+))
        , ("-", stackOp2(-))
        , ("*", stackOp2(*))
        , ("neg", stackOp1 negate)
        , ("dup", dup)
        , ("=", dup)
        , ("swap", swap)
        , ("`", swap)
        , ("drop", drop1)
        , ("?", drop1)
        ]

genericUndo :: Engine a b -> [Undo a b] -> [Undo a b] -> (Engine a b, [Undo a b], [Undo a b])
genericUndo engine ((Undo undos):rest) redos = 
    let doUndos (e, us) f = let (e1, Undo u) = f e in (e1, u ++ us)
        (newEngine, redo) = foldl doUndos (engine, []) undos
    in (newEngine, rest, (Undo redo):redos)
genericUndo engine [] redos = (engine, [], redos)

calcUndo :: Calculator a b -> Calculator a b
calcUndo c =
    let (e, u, r) = genericUndo (calcEngine c) (calcUndos c) (calcRedos c)
    in c{ calcEngine = e, calcUndos=u, calcRedos=r }

calcRedo :: Calculator a b -> Calculator a b
calcRedo c =
    let (e, r, u) = genericUndo (calcEngine c) (calcRedos c) (calcUndos c)
    in c{ calcEngine = e, calcUndos=u, calcRedos=r }

data Calculator a b = Calculator
    { calcEngine :: Engine a b
    , calcUndos :: [Undo a b]
    , calcRedos :: [Undo a b]
    , calcOp :: String -> Maybe (EngineFn a b)
    , calcReads :: String -> [(a, String)]
    , calcDisp :: Calculator a b -> IO()
    }

calcDisplay :: Calculator a b -> IO()
calcDisplay c = (calcDisp c) c

calcApply :: Calculator a b -> EngineFn a b -> Calculator a b
calcApply calc f =
    let eng = calcEngine calc
        (newEng, undo) = f eng
        undos = case undo of
            NoUndo -> calcUndos calc
            _  -> undo:(calcUndos calc)
    in calc{ calcEngine=newEng, calcUndos=undos, calcRedos=[] }
