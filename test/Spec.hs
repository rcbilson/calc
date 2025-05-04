import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Calculator" $ do
  -- Test specifications will be imported from CalculatorSpec.hs