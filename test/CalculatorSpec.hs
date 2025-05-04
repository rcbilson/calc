import Test.Hspec
import Calculator

main :: IO ()
main = hspec $ do
  describe "Calculator" $ do
    it "adds two numbers" $ do
      add 1 2 `shouldBe` 3

    it "subtracts two numbers" $ do
      subtract 5 3 `shouldBe` 2

    it "multiplies two numbers" $ do
      multiply 2 3 `shouldBe` 6

    it "divides two numbers" $ do
      divide 6 2 `shouldBe` 3
      divide 5 0 `shouldThrow` anyException