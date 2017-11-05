{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Exception
import           Control.Monad       (forM_, (<=<))
import           Control.Monad.Loops (iterateM_)
import           LogLine
import           Stats               hiding (numLines)
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure, exitSuccess)
import           System.Random       (randomRIO)
import           Text.Printf         (printf)
import           Units               (DistUnit (..), TempUnit (..))

data RunMode =
    HelpMode
  | TestGenMode
  | AnalyseMode
  | NormalizeMode
  | InvalidMode

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    HelpMode      -> showHelp
    TestGenMode   -> runTestGenMode $ tail args
    AnalyseMode   -> runAnalyseMode $ tail args
    NormalizeMode -> runNormalizeMode $ tail args
    InvalidMode   -> warnInvalidMode args

-- TODO have a short and a long variant of the help message (-h vs. --help)
-- a la `git`.
helpMessage :: String
helpMessage = unlines [
    "Usage: wbstat ( [-h|--help]"
  , "              | [-t|--testgen] <lines>"
  , "              | [-n|--normalize] <k(elvin)|f(ahrenheit)|c(elsius)> <km|mi>"
  , "              | [-a|--analyse])" -- analyse spits out data atm, TODO make selectable.
  ]

-- Get which runtime mode is specified by the flags.
parseArgs :: [String] -> RunMode
parseArgs ["-h"]                = HelpMode
parseArgs ["--help"]            = HelpMode
parseArgs ["-t", _]             = TestGenMode
parseArgs ["--testgen", _]      = TestGenMode
parseArgs ["-n", _, _]          = NormalizeMode
parseArgs ["--normalize", _, _] = NormalizeMode
parseArgs ["-a"]                = AnalyseMode
parseArgs ["--analyse"]         = AnalyseMode
parseArgs _                     = InvalidMode

showHelp :: IO ()
showHelp = do
  putStr helpMessage
  exitSuccess

runTestGenMode :: [String] -> IO ()
runTestGenMode args = do
  let numLines = read (head args) :: Int
  seed <- logLineSeed
  iterateM_ go (numLines, seed)
  where
    go :: (Int, LogLine) -> IO (Int, LogLine)
    go (n, ll) =
      if n == 0
        then exitSuccess
        else do
          r1   <- randomRIO (0 :: Int, 165) -- 33% of 500: arbitrarily chosen. XXX
          r2   <- randomRIO (1 :: Int, 100)
          ll' <- if r1 == 0
                    then if (n `mod` r2) == 0 -- Ocassionally change the code.
                          then stepLogLine True ll
                          else stepLogLine False ll
                    else delayedLogLine =<< stepLogLine False ll
          print ll'
          return (n-1, ll')

runNormalizeMode :: [String] -> IO ()
runNormalizeMode args = do
  ls <- getContents
  forM_ (lines ls) (\l -> catch
                            (print <=< normalizeLogLine t d <=< parseLogLine $ l)
                            handler)
    where t = case head args of
                "k" -> Kelvin
                "f" -> Fahrenheit
                "c" -> Celsius
                o   -> error $ "Invalid temperature unit " ++ "\"" ++ o ++ "\""
          d = case args !! 1 of
                "km" -> Kilometers
                "mi" -> Miles
                o    -> error $ "Invalid distance unit " ++ "\"" ++ o ++ "\""
          handler :: SomeException -> IO ()
          handler _ = return () -- Could be used to exitFailure on bad data
                                -- or be verbose (e.g. to a log)

runAnalyseMode :: [String] -> IO ()
runAnalyseMode _ = do
  logLines      <- getContents
  (stats, _) <- iterateWhile' (not . null . snd) (empty, lines logLines) go
  showStats stats
  where go (s, ls) = catch process handler
          where process = parseLogLine (head ls)
                            >>= analyseLogLine s
                              >>= (\s' -> return (s', tail ls))
                handler :: SomeException -> IO (Stats, [String])
                handler _ = return (s, tail ls) -- Could be used to exitFailure on bad data
                                                -- or be verbose (e.g. to a log)

warnInvalidMode :: [String] -> IO ()
warnInvalidMode args
  | null args = do
    putStrLn "Please specify a mode for wbstat"
    exitFailure
  | otherwise = do
    printf "Invalid option %s\n" (show $ head args)
    exitFailure

-- Strict variant of iterateWhile.
iterateWhile' :: Monad m => (a -> Bool) -> a -> (a -> m a) -> m a
iterateWhile' cond initialState f = go initialState
  where
    go !state | cond state = f state >>= go
              | otherwise  = return state
