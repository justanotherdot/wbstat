module LogLine
  ( LogLine (..)
  , Coordinate (..)
  , Timestamp (..)
  , logLineSeed
  , parseLogLine
  , mangleLogLine
  , delayedLogLine
  , stepLogLine
  , ParseException (..)
  ) where

import           Control.Exception (Exception, throw)
import           Control.Monad     (forM)
import           Data.List         (intercalate)
import           Data.List.Split   (splitOn)
import           Data.Time
import           Data.Time.Clock   (UTCTime (..))
import           Data.Typeable     (Typeable)
import           Obsv
import           System.Random     (randomRIO)

newtype Timestamp  = Timestamp (String, UTCTime) deriving Eq
newtype Coordinate = Coordinate (Integer, Integer) deriving Eq
type Temperature   = Integer
type ObsvCode      = String
data LogLine       = LogLine Timestamp Coordinate Temperature ObsvCode deriving Eq

data ParseException =
      InvalidLogLineException
    | InvalidCoordinateException
    | InvalidTimestampException deriving (Show, Typeable)

instance Exception ParseException

instance Show LogLine where
  show (LogLine time coords temp obsv) =
    intercalate "|" [show time, show coords, show temp, obsv]

instance Show Timestamp where
  show (Timestamp (s, _)) = s

instance Show Coordinate where
  show (Coordinate (x, y)) = show x ++ "," ++ show y

-- Initial log line seed to feed into stepping function.
logLineSeed :: IO LogLine
logLineSeed = do
  date   <- randomTimestamp
  randX  <- randomRIO (0, coordXBound) -- kilometers
  randY  <- randomRIO (0, coordYBound)
  tmp    <- randomRIO (0, 333) -- kelvin
  code   <- randObsvCode
  let coord = Coordinate (randX, randY)
  return $ LogLine date coord tmp code

-- Randomly deform a LogLine involving damaged text or reversed time mimicking
-- data that showed up in the wrong order or has been trasnmitted with errors.
mangleLogLine :: String -> IO String
mangleLogLine cs =
  forM cs (\c -> do
            randInt   <- randomRIO (0 :: Int, 9)
            randInt'  <- randomRIO (0 :: Int, 1)
            randCharLow  <- randomRIO ('a', 'z')
            randCharCap <- randomRIO ('A', 'Z')
            if randInt == 0
              then if randInt' == 0
                    then return randCharLow
                    else return randCharCap
              else return c)

-- NOTE One could probably encode this into stepLogLine if one isn't concerned.
-- about more frequent 'delayed' log lines.
-- Mimick's a log line that arrived in a non-monoticically increasing order.
delayedLogLine :: LogLine -> IO LogLine
delayedLogLine (LogLine date coord temp code) = do
  timeDelta <- randomRIO (60*30 :: Int, 60*120)
  date' <- genTimestamp date (realToFrac (negate timeDelta) :: NominalDiffTime)
  return $ LogLine date' coord temp code

-- TODO needs to check we don't go before 1896
-- Transforms a log line into a new intance with reasonable differences applied.
-- Also allows for an occasional chance that a log line can be mangled.
stepLogLine :: Bool -> LogLine -> IO LogLine
stepLogLine changeCode (LogLine date (Coordinate (x, y)) temp code) = do
  timeDelta <- randomRIO (60 :: Double, 60*60)
  date'     <- genTimestamp date (realToFrac timeDelta :: NominalDiffTime)
  xDelta    <- randomRIO (-320 :: Integer, 320) -- Supposed weather balloon speed in kmh.
  yDelta    <- randomRIO (-320 :: Integer, 320)
  tempDelta <- randomRIO (-30 :: Integer, 30)
  code'     <- if changeCode then randObsvCode else return code
  return (LogLine date' (Coordinate (x + xDelta, y + yDelta)) (temp + tempDelta) code')

-- Time elapsed in seconds from January 1st, 1896 and the current time.
-- Date recorded for first weather balloons approx. 1896
-- Assuming January 1st for simplicity.
timeLowerBound :: IO Double
timeLowerBound = do
  now <- getCurrentTime
  return . realToFrac . diffUTCTime now $ dateLowerBound
    where dateLowerBound = UTCTime (fromGregorian 1896 1 1) 0

-- 100 years into the future.
timeUpperBound :: IO Double
timeUpperBound = do
  now <- getCurrentTime
  let dateUpperBound = addUTCTime (60*60*24*365*100 :: NominalDiffTime) now
  return . realToFrac . diffUTCTime now $ dateUpperBound

-- Approximate distance of equator in kilometers.
coordXBound :: Integer
coordXBound = 40030

-- Two times the distance from pole to pole.
coordYBound :: Integer
coordYBound = 40008

randomTimestamp :: IO Timestamp
randomTimestamp = do
  lowerBound      <- timeLowerBound
  upperBound      <- timeUpperBound
  randDiffSeconds <- randomRIO (negate lowerBound, upperBound)
  now             <- getCurrentTime
  genTimestamp (Timestamp ("", now)) (realToFrac randDiffSeconds :: NominalDiffTime)

timestampFormat :: String
timestampFormat = iso8601DateFormat (Just "%H:%M")

genTimestamp :: Timestamp -> NominalDiffTime -> IO Timestamp
genTimestamp (Timestamp (_, date)) delta = do
  let time = addUTCTime delta date
  return $ Timestamp (formatTime defaultTimeLocale timestampFormat time, time)

parseTimestamp :: String -> Timestamp
parseTimestamp s =
  case parse s of
    Just s' -> Timestamp (s, s')
    Nothing -> throw InvalidTimestampException "Invalid string passed to parseTimestamp"
  where
    parse :: String -> Maybe UTCTime
    parse = parseTimeM True defaultTimeLocale timestampFormat

parseCoordinate :: String -> Coordinate
parseCoordinate = readCoord . splitOn ","
  where readCoord [x, y] = Coordinate (read x :: Integer, read y :: Integer)
        readCoord _      = throw InvalidCoordinateException

-- Turn a string representing a log line into a LogLine
parseLogLine :: String -> IO LogLine
parseLogLine s
  | length ps /= 4 = throw InvalidLogLineException
  | otherwise = do
              let timestamp = parseTimestamp $ head ps
              let coord     = parseCoordinate $ ps !! 1
              let temp      = read (ps !! 2) :: Integer
              let code      = ps !! 3
              return $ LogLine timestamp coord temp code
  where
    ps = splitOn "|" s
