module CalculatorClass ( CalculatorClass, calcDisplay, calcConsume ) where
class CalculatorClass a where
    calcDisplay :: a -> IO()
    calcConsume :: a -> String -> (a, String)
