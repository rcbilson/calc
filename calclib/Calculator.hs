module Calculator (
    Calculator,
    calcDisplay,
    calcConsume,
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
    Undo( Undo ),
    numericOps,
    genericConsume,
    genericUndo) where

-- A Calculator is something that consumes string input and displays itself
class Calculator a where
    -- calcDisplay outputs a textual representation of the calculator state.
    -- The expectation is that this is 5 lines, one for operational state and
    -- four for the top four stack entries.
    calcDisplay :: a -> IO()

    -- calcConsume takes a Calculator and a String; it is expected to take
    -- any prefix of the string that indicates a valid datum or operation
    -- and update the state of the calculator accordingly. It returns the
    -- new state of the calculator and the remainder of the string.
    calcConsume :: a -> String -> (a, String)

    calcUndo :: a -> a

    calcRedo :: a -> a

-- The rest of this file are utility classes that are common to the different
-- Calculator implementations.

-- A stack is represented as a list, with the head of the list as top of stack
type Stack a = [a]

-- ensureStack takes a stack and produces an equivalent stack that is at least
-- four elements deep, filling in with zeroes at the base of the stack as
-- necessary.
ensureStack :: Num a => Stack a -> Stack a
ensureStack stk
    | length(stk) < 4 = ensureStack (stk ++ [0])
    | otherwise       = stk

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
data Undo a b = Undo [EngineFn a b]

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
opStateOp f (Engine stk ops) = (Engine stk (f ops), Undo [])

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

-- genericConsume is a function that can be used to implement calcConsume.
-- It is parameterized with two functions:
--   - lookupOp takes a string and may return a corresponding EngineFn
--   - readNum reads a numeric value as a prefix of a string
--     (It's just "reads" but you can't really use "reads" directly in a
--     polymorphic context.)
-- The result of applying these two functions is a function that takes
-- an Engine and a String, then decides if there is a prefix of that string
-- that represents a valid number or a valid operation. If there is a
-- valid number the number is pushed to the stack. If there is a valid
-- operation the operation is applied to the Engine. It returns the updated
-- engine, any remaining part of the string, and a list of Undos.
genericConsume :: (String -> Maybe (EngineFn a b)) -> (String -> [(a, String)]) -> Engine a b -> String -> (Engine a b, String, [Undo a b])
-- Special case: if it's 0x or 0X it could be the beginning of
-- a valid hex number so let it ride.
genericConsume _ _ eng "0x" = (eng, "0x", [])
genericConsume _ _ eng "0X" = (eng, "0X", [])
-- Allow _ to stand in for prefix negation
genericConsume _ _ eng "_"  = (eng, "-", [])
genericConsume lookupOp readNum eng str = 
    case readNum str of
        -- Special case: if it's a valid number with nothing or a dot
        -- following it could continue on to be a bigger number, so
        -- continue accumulating characters.
        (_, ""):_   -> (eng, str, [])
        (_, "."):_  -> (eng, str, [])
        (_, ":"):_  -> (eng, str, [])
        (num, rest):_ ->
            -- special case: if the character caused a token to be returned, it might
            -- be the case that the remainder is also a valid token (consider the input
            -- "123+", the + causes the number to be completed as a token but it is
            -- itself a token.
            let (newEng, undo1) = push num eng
                (finalEng, finalRest, undo2) = genericConsume lookupOp readNum newEng rest
            in (finalEng, finalRest, undo2 ++ [undo1])
        [] -> case lookupOp str of
            Just f ->
                let (r, u) = f eng
                    us = case u of
                        Undo [] -> []
                        x -> [x]
                in (r, "", us)
            Nothing -> (eng, str, [])

genericUndo :: Engine a b -> [Undo a b] -> [Undo a b] -> (Engine a b, [Undo a b], [Undo a b])
genericUndo engine ((Undo undos):rest) redos = 
    let doUndos (e, us) f = let (e1, Undo u) = f e in (e1, u ++ us)
        (newEngine, redo) = foldl doUndos (engine, []) undos
    in (newEngine, rest, (Undo redo):redos)
genericUndo engine [] redos = (engine, [], redos)
