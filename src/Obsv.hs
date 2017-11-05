module Obsv
    ( knownObsvCodes
    , observatory
    , randObsvCode
    ) where

import qualified Data.Map.Strict as Map
import           System.Random   (randomRIO)
import           Units           (DistUnit (..), TempUnit (..))

-- TODO Could be a newtype ...
type ObsvCode = String

-- All known observatories
observatories :: Map.Map ObsvCode (TempUnit, DistUnit)
observatories = Map.fromList [
    ("AU", (Celsius, Kilometers))
  , ("US", (Fahrenheit, Miles))
  , ("FR", (Kelvin, Kilometers))
  ]

observatory :: ObsvCode -> (TempUnit, DistUnit)
observatory code = Map.findWithDefault (Kelvin, Kilometers) code observatories

-- All known observatory codes
knownObsvCodes :: [ObsvCode]
knownObsvCodes = Map.keys observatories

-- Provide a random two-character observatory code
randUnknownObsvCode :: IO ObsvCode
randUnknownObsvCode = do
  randIx1 <- randomRIO (0, length letters - 1)
  randIx2 <- randomRIO (0, length letters - 1)
  let randCode = (letters !! randIx1) : [letters !! randIx2]
  if randCode `notElem` knownObsvCodes
    then return randCode
    else randUnknownObsvCode
  where
    letters = ['A'..'Z']

randObsvCode :: IO ObsvCode
randObsvCode = do
  r1 <- randUnknownObsvCode
  r2 <- randUnknownObsvCode
  r3 <- randUnknownObsvCode
  let codes = knownObsvCodes ++ [r1, r2, r3]
  randIx <- randomRIO (0, length codes - 1)
  return (codes !! randIx)
