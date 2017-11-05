module Stats
    ( normalizeLogLine
    , analyseLogLine
    , Stats (..)
    , empty
    , showStats
    ) where

import           Control.Monad   (forM_)
import qualified Data.Map.Lazy   as Map
import           Data.Maybe
import           Data.Time       (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Error           (ProgramError (..))
import           LogLine         (Coordinate (..), LogLine (..), Timestamp (..))
import           Obsv
import           Units

data Stats = Stats { minTemp  :: Integer
                   , maxTemp  :: Integer
                   , sumTemp  :: Integer
                   , numObsvs :: Map.Map String Integer -- No. of observations per observatory
                   , begCoord :: (UTCTime, Coordinate)
                   , endCoord :: (UTCTime, Coordinate)
                   , numLines :: Integer
                   } deriving Show

empty :: Stats
empty = Stats { numLines = 0
              , minTemp  = 0
              , maxTemp  = 0
              , sumTemp  = 0
              , numObsvs = Map.empty
              , begCoord = (initUTCTime, Coordinate (0, 0))
              , endCoord = (initUTCTime, Coordinate (0, 0))
              }
  where initUTCTime = UTCTime (fromGregorian 1896 1 1) 0

-- Normalize a log line's values to desired units.
normalizeLogLine :: TempUnit -> DistUnit -> LogLine -> IO LogLine
normalizeLogLine wantTemp wantDist (LogLine ts (Coordinate (x, y)) temp code) = do
  let (x', y') = (normDist x, normDist y)
  let temp'       = normTemp temp
  return $ LogLine ts (Coordinate (x', y')) temp' code
    where
      (tempUnit, distUnit) = observatory code
      toDouble n   = fromIntegral n :: Double
      normTemp t   = truncate $ convertTemp (toDouble t) tempUnit wantTemp
      normDist d   = if wantDist /= distUnit
                      then truncate $ convertDist distUnit (toDouble d) else d

-- Euclidean distance between two points.
euclidDist :: Floating a => (Integer, Integer) -> (Integer, Integer) -> a
euclidDist (x0, y0) (x1, y1) = sqrt $ fromIntegral (x + y)
  where x = (x1 - x0)^(2 :: Integer)
        y = (y1 - y0)^(2 :: Integer)

meanAvg :: Integer -> Integer -> Either ProgramError Integer
meanAvg _ 0 = Left $ InvalidArgs "Division by zero"
meanAvg t n = Right $ truncate (realToFrac $ (fromIntegral t :: Double) / fromIntegral n :: Double)

analyseLogLine :: Stats -> LogLine -> IO Stats
analyseLogLine stats logLine = do
  LogLine date coord temp code <- normalizeLogLine Kelvin Kilometers logLine
  let Timestamp (_, utcTime)       = date
  -- Clean this up with Control.Monad.Arrow
  let analyse = newEndCoord utcTime coord
              . newBegCoord utcTime coord
              . incObsvCount code
              . newSumTemp temp
              . newMaxTemp temp
              . newMinTemp temp
              . incNumLines
  return $ analyse stats

-- Increment line processed count.
incNumLines :: Stats -> Stats
incNumLines stats = stats { numLines = numLines' }
  where numLines' = numLines stats + 1

newMinTemp :: Integer -> Stats -> Stats
newMinTemp sample stats
  | sample < minTemp stats = stats { minTemp = sample }
  | otherwise              = stats

newMaxTemp :: Integer -> Stats -> Stats
newMaxTemp sample stats
  | sample > maxTemp stats = stats { maxTemp = sample }
  | otherwise              = stats

-- Calculate a new mean average.
newSumTemp :: Integer -> Stats -> Stats
newSumTemp sample stats
  | numLines stats <= 1 = stats { sumTemp = sample }
  | otherwise           = stats { sumTemp = sample + sumTemp stats }

incObsvCount :: String -> Stats -> Stats
incObsvCount code stats = stats { numObsvs = numObsvs' }
  where numObsvs' = Map.insertWith (+) code 1 (numObsvs stats)

newBegCoord :: UTCTime -> Coordinate -> Stats -> Stats
newBegCoord sampleTime coord stats
  | sampleTime < oldTime = stats { begCoord = (sampleTime, coord) }
  | otherwise            = stats
  where (oldTime, _) = begCoord stats

newEndCoord :: UTCTime -> Coordinate -> Stats -> Stats
newEndCoord sampleTime coord stats
  | sampleTime > oldTime = stats { endCoord = (sampleTime, coord) }
  | otherwise            = stats
  where (oldTime, _) = endCoord stats

showTotalDistance :: Stats -> IO ()
showTotalDistance stats = putStrLn $ show total ++ " km traversed"
  where
    (_, Coordinate x0y0) = begCoord stats
    (_, Coordinate x1y1) = endCoord stats
    total :: Integer
    total = truncate (euclidDist x1y1 x0y0 :: Double)

showObservations :: Stats -> IO ()
showObservations stats = forM_ obsvs (putStrLn . showIt)
  where
    lookupObsvs k  = fromMaybe (-1) (Map.lookup k (numObsvs stats))
    obsvs          = Map.keys $ numObsvs stats
    showIt       k = show k ++ " had " ++ show (lookupObsvs k) ++ " observations recorded"

showStats :: Stats -> IO ()
showStats stats = do
  putStrLn $ show (numLines stats) ++ " log lines processed"
  putStrLn $ show (minTemp stats) ++ " lowest temp in kelvins"
  putStrLn $ show (maxTemp stats) ++ " highest temp in kelvins"
  putStrLn $ show avg  ++ " mean average of temperature"
  showObservations stats
  showTotalDistance stats
  where
    avg = case meanAvg (sumTemp stats) (numLines stats) of
            Right             v  -> v
            Left (InvalidArgs m) -> 0 -- Dividing by zero implies no log lines supplied.
