import Test.Hspec

import Runner
import Calculator
--import IntegerCalculator
import DoubleCalculator
import Data.Word

-- test a string of commands, returning the top of stack
{-
doIntegerTest :: String -> Integer
doIntegerTest input =
    let startCalc = makeIntegerCalculator [0,0,0,0]
        endCalc = testCalculator startCalc "" input
    in case endCalc of IntegerCalculator (Engine (x:xs) _) _ _ -> x

doWord8Test :: String -> Word8
doWord8Test input =
    let startCalc = makeWord8Calculator [0,0,0,0]
        endCalc = testCalculator startCalc "" input
    in case endCalc of Word8Calculator (Engine (x:xs) _) _ _ -> x

doWord16Test :: String -> Word16
doWord16Test input =
    let startCalc = makeWord16Calculator [0,0,0,0]
        endCalc = testCalculator startCalc "" input
    in case endCalc of Word16Calculator (Engine (x:xs) _) _ _ -> x

doWord32Test :: String -> Word32
doWord32Test input =
    let startCalc = makeWord32Calculator [0,0,0,0]
        endCalc = testCalculator startCalc "" input
    in case endCalc of Word32Calculator (Engine (x:xs) _) _ _ -> x

doWord64Test :: String -> Word64
doWord64Test input =
    let startCalc = makeWord64Calculator [0,0,0,0]
        endCalc = testCalculator startCalc "" input
    in case endCalc of Word64Calculator (Engine (x:xs) _) _ _ -> x
-}
doDoubleTest :: String -> Double
doDoubleTest input =
    let startCalc = makeDoubleCalculator [0,0,0,0]
        endCalc = testCalculator startCalc "" input
    in case endCalc of DoubleCalculator (Engine (x:xs) _) _ _ -> x

main :: IO ()
main = hspec spec

numericTests doTest = do
    it "adds two numbers" $ do
      doTest "1 2+" `shouldBe` 3

    it "adds two numbers with an extra space" $ do
      doTest "1 2 +" `shouldBe` 3

    it "adds many numbers" $ do
      doTest "1 2 3 4 5 6 7 8 9++++++++" `shouldBe` 45

    it "subtracts two numbers" $ do
      doTest "1 2-" `shouldBe` -1

    it "multiplies two numbers" $ do
      doTest "3 2*" `shouldBe` 6

    it "negates a number" $ do
      doTest "17neg" `shouldBe` -17

    it "swaps two numbers" $ do
      doTest "2 3swap" `shouldBe` 2

    it "drops a number" $ do
      doTest "2 3swap drop" `shouldBe` 3

    it "duplicates a number" $ do
      doTest "2 dup drop" `shouldBe` 2

    it "undoes a unary operation" $ do
      doTest "17negu" `shouldBe` 17

integerTests doTest = do
    (numericTests doTest)

    it "divides two numbers" $ do
      doTest "17 7/" `shouldBe` 2

    it "computes a remainder" $ do
      doTest "17 7%" `shouldBe` 3

    it "computes integer powers" $ do
      doTest "2 3^" `shouldBe` 8

    it "computes bitwise and" $ do
      doTest "5 10&" `shouldBe` 0

    it "computes bitwise or" $ do
      doTest "5 10|" `shouldBe` 15

    it "computes bitwise xor" $ do
      doTest "5 11~" `shouldBe` 14

    it "computes bitwise not" $ do
      doTest "0x55 0xf!&" `shouldBe` 0x50

    it "shifts multiple places left" $ do
      doTest "0x07 4 shl" `shouldBe` 0x70

    it "shifts multiple places right" $ do
      doTest "0x70 4 shr" `shouldBe` 0x07

    it "shifts one place left" $ do
      doTest "0x6<" `shouldBe` 0x0c

    it "shifts one place right" $ do
      doTest "0xc>" `shouldBe` 0x06

    it "sets a bit" $ do
      doTest "3sb" `shouldBe` 0x8

    it "clears a bit" $ do
      doTest "12 2cb" `shouldBe` 0x8


spec :: Spec
spec = do
{-
    describe "IntegerCalculator" $ do
        (integerTests doIntegerTest)

    describe "Word8Calculator" $ do
        (integerTests doWord8Test)

        it "truncates to 8 bits" $ do
            doWord8Test "0xff 1+" `shouldBe` 0

        it "uses 2's complement negation" $ do
            doWord8Test "0xff neg" `shouldBe` 1

    describe "Word16Calculator" $ do
        (integerTests doWord16Test)

        it "truncates to 16 bits" $ do
            doWord16Test "0xffff 1+" `shouldBe` 0

        it "uses 2's complement negation" $ do
            doWord16Test "0xffff neg" `shouldBe` 1

    describe "Word32Calculator" $ do
        (integerTests doWord32Test)

        it "truncates to 32 bits" $ do
            doWord32Test "0xffffffff 1+" `shouldBe` 0

        it "uses 2's complement negation" $ do
            doWord32Test "0xffffffff neg" `shouldBe` 1

    describe "Word64Calculator" $ do
        (integerTests doWord64Test)

        it "truncates to 64 bits" $ do
            doWord64Test "0xffffffffffffffff 1+" `shouldBe` 0

        it "uses 2's complement negation" $ do
            doWord64Test "0xffffffffffffffff neg" `shouldBe` 1
-}
    describe "DoubleCalculator" $ do
        (numericTests doDoubleTest)

        it "divides two numbers" $ do
            doDoubleTest "17 2/" `shouldBe` 8.5

        it "raises to an integer power" $ do
            doDoubleTest "14 2^" `shouldBe` 196

        it "raises to a fractional power" $ do
            doDoubleTest "196 0.5^" `shouldBe` 14

        it "computes natural logarithm and exponential" $ do
            doDoubleTest "1 ln exp exp ln" `shouldBe` 1

        it "computes logarithm to base 2" $ do
            doDoubleTest "1024 lg" `shouldBe` 10

        it "computes logarithm to arbitrary base" $ do
            doDoubleTest "1024 2 log" `shouldBe` 10

        it "computes sin and arcsin" $ do
            doDoubleTest "1 sin asin" `shouldBe` 1

        it "computes cos and arccos" $ do
            -- use 2 on account of precision
            doDoubleTest "2 cos acos" `shouldBe` 2

        it "computes tan and arctan" $ do
            doDoubleTest "1 tan atan" `shouldBe` 1
