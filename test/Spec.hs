module Main where

import           Data.Char               (toUpper)
import           Obsv
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Gen     (oneof)
import           Test.QuickCheck.Monadic
import           TestGen
import           Units

tempUnitGen :: Gen TempUnit
tempUnitGen =
  elements [Fahrenheit, Celsius, Kelvin]

distUnitGen :: Gen DistUnit
distUnitGen = oneof [ return Kilometers, return Miles ]

-- Rounds a given floating point number to n many places after the decimal
roundTo :: (Integral a, RealFrac b) => a -> b -> b
roundTo n v = (/10.0^^n) . fromIntegral $ rounded
  where rounded = round (v * 10^n) :: Integer

main :: IO ()
main = hspec $ do
  describe "Unit" $ do
    describe "convertTemp" $ do
      it "correctly converts 1 unit of fahrenheit to celsius" $ do
        let expected = -17.2222 :: Double
        let actual   = roundTo (4 :: Int) (convertTemp 1 Fahrenheit Celsius :: Double)
        actual `shouldBe` expected
      it "correctly converts 1 unit of fahrenheit to kelvin" $ do
        let expected = 255.9278 :: Double
        let actual   = roundTo (4 :: Int) (convertTemp 1 Fahrenheit Kelvin :: Double)
        actual `shouldBe` expected
      it "correctly converts 1 unit of celsius to fahrenheit" $ do
        let expected = 33.8 :: Double
        let actual   = roundTo (4 :: Int) (convertTemp 1 Celsius Fahrenheit :: Double)
        actual `shouldBe` expected
      it "correctly converts 1 unit of celsius to kelvin" $ do
        let expected = 274.15 :: Double
        let actual   = roundTo (4 :: Int) (convertTemp 1 Celsius Kelvin :: Double)
        actual `shouldBe` expected
      it "correctly converts 1 unit of kelvin to fahrenheit" $ do
        let expected = (-457.87) :: Double
        let actual   = roundTo (4 :: Int) (convertTemp 1 Kelvin Fahrenheit :: Double)
        actual `shouldBe` expected
      it "correctly converts 1 unit of kelvin to celsius" $ do
        let expected = (-272.15) :: Double
        let actual   = roundTo (4 :: Int) (convertTemp 1 Kelvin Celsius :: Double)
        actual `shouldBe` expected
      it "is idempotent when the desired unit matches the given" $ do
        let actual = roundTo (4 :: Int) (convertTemp 1 Kelvin Kelvin :: Double)
        actual `shouldBe` 1
    describe "convertDist" $ do
      it "correctly converts kilometers to miles" $ do
        let actual   = roundTo (6 :: Int) $ convertDist Kilometers 1 :: Double
        let expected = 0.621371
        actual `shouldBe` expected
      it "correctly converts miles to kilometers" $ do
        let actual   = roundTo (6 :: Int) $ convertDist Miles 1 :: Double
        let expected = 1.609344
        actual `shouldBe` expected
  describe "TestGen" $ do
    it ""
      property $ 1 == 1
