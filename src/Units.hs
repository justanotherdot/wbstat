module Units
  ( TempUnit (..)
  , DistUnit (..)
  , convertTemp
  , convertDist
  ) where

data TempUnit =
    Fahrenheit
  | Celsius
  | Kelvin
  deriving (Eq, Show)

data DistUnit =
    Kilometers
  | Miles
  deriving (Eq, Show)

-- Converts a value of the first temperature unit to the second unit.
convertTemp :: RealFrac a => a -> TempUnit -> TempUnit -> a
convertTemp val Fahrenheit Celsius    = (/9) . (*5) $ (val - 32)
convertTemp val Fahrenheit Kelvin     = (+273.15) . (/9) . (*5) $ (val - 32)
convertTemp val Celsius    Fahrenheit = (+32) . (/5) . (*9) $ val
convertTemp val Celsius    Kelvin     = val + 273.15
convertTemp val Kelvin     Fahrenheit = (\x' -> x' - 459.67) . (/5) . (*9) $ val
convertTemp val Kelvin     Celsius    = val - 273.15
convertTemp val _          _          = val -- Must be same constructors.

-- If kilometers, will return miles, and vice versa.
-- This should be refactored like `convertTemp` in the need that more
-- units are needed.
convertDist :: RealFrac a => DistUnit -> a -> a
convertDist unit val =
  case unit of
    Kilometers -> let km = val in km * c -- to miles
    Miles      -> let mi = val in mi / c -- to kilometers
  where c = 0.621371
