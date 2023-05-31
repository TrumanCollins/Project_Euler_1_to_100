{-
  Solution to Project Euler problems.
  Solved by Truman Collins
  April 19, 2014 to ...
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# Language BinaryLiterals #-}

import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import System.Clock
import System.Console.GetOpt
import System.IO
import System.Environment (getArgs, getProgName)
import System.Exit
import Text.Printf
import Control.Monad
import PrimeMod
import Euler1to100

version :: String
version = "1.0"

-- Helper functions for time calculations.
    
convertTimeToDouble :: TimeSpec -> Double
convertTimeToDouble tm = fromIntegral (sec tm) + fromIntegral (nsec tm) / 1.0e9

computeElapsedTime :: TimeSpec -> TimeSpec -> Double
computeElapsedTime startTime endTime 
  = convertTimeToDouble endTime - convertTimeToDouble startTime

-- The timer functions return two indications, whether the answer was checked, and if so, whether
-- there was an error with the answer. I had each return a pair, but in this change, either here or
-- in the caller, I had a segfault when running threaded. I don't know if it was because of this or
-- because I did an update of the Haskell system at the same time. I simplified this by having them
-- return an int with the first bit indicating unchecked (1) or checked (0), and the second bit
-- indicating an error (1). So there are three possible return values: 0 (checked and result is
-- correct), 1 (the result was unchecked), and 2 (checked and the result is incorrect).

-- Shorthand names for timer functions.

tfICk :: (Integral a, Show a) => a -> Integer -> String -> IO Int
tfICk fn answer = timeFunctionIntegral fn (True, answer)
tfIOCk :: (Integral a, Show a) => IO a -> Integer -> String -> IO Int
tfIOCk fn answer = timeFunctionIO fn (True, answer)

-- Call the given function; time how long it takes to run; print the result; if asked, compare with
-- the given answer and generate an error if not the same; and then print the time taken.
-- Versions for functions returning integral values, double values, and those that use IO.

oneIfUnchecked :: Bool -> Int
oneIfUnchecked check
  | check = 0
  | otherwise = 1

timeFunctionIntegral :: (Integral a, Show a) => a -> (Bool, Integer) -> String -> IO Int
timeFunctionIntegral fn (check, answer) probName = do
  (diff, probResult) <- printProbAndTimeFunc fn probName
  showResultAndCheckError probResult answer diff check

timeFunctionIO :: (Integral a, Show a) => IO a -> (Bool, Integer) -> String -> IO Int
timeFunctionIO fn (check, answer) probName = do
  printAndFlushProbName probName
  startTime <- getTime Realtime
  probResult <- fn
  endTime <- probResult `seq` getTime Realtime
  let diff = computeElapsedTime startTime endTime
  showResultAndCheckError probResult answer diff check

-- Helper functions for the above three timing functions.

showResultAndCheckError :: (Integral a, Show a) => a -> Integer -> Double -> Bool -> IO Int
showResultAndCheckError probResult answer diff check = do
  putStr $ show probResult
  oneIfError <- checkAnswerAndPrintTime check (fromIntegral probResult == answer) diff (show answer)
  let result = oneIfError + oneIfError + oneIfUnchecked check
  return result

printProbAndTimeFunc :: a -> String -> IO (Double, a)
printProbAndTimeFunc fn probName = do
  printAndFlushProbName probName
  startTime <- getTime Realtime
  let result = fn
  endTime <- result `seq` getTime Realtime
  let diff = computeElapsedTime startTime endTime
  return (diff, result)

printAndFlushProbName :: String -> IO ()
printAndFlushProbName probName = do
  putStr $ probName ++ ": "
  hFlush stdout  

checkAnswerAndPrintTime :: Bool -> Bool -> Double -> String -> IO Int
checkAnswerAndPrintTime check compareWithAnswerOkay timeDiff answerStr = do
  let oneIfError = if not check || compareWithAnswerOkay then 0 else 1
  when (oneIfError == 1) (putStr $ "  ERROR: Answer is " ++ answerStr)
  putStrLn ""
  printf "  time: %0.6f sec.\n" timeDiff
  return oneIfError

-- Call the given function, time how long it takes, and then the time taken.  Do as little of the
-- printing operation as possible while the timer is running.

timeIOFunctionHRNoPrint :: (Show a) => IO a -> String -> IO ()
timeIOFunctionHRNoPrint fn probName = do
  printAndFlushProbName probName
  startTime <- getTime Realtime
  _ <- fn
  endTime <- getTime Realtime
  putStrLn ""
  let diff = computeElapsedTime startTime endTime
  printf "  time: %0.6f sec.\n" (diff :: Double)

-- This function will take an element of the list of functions to run, and run it if either all
-- problem functions should be run, if this function's ID number is on the list of those to be run,
-- or if there is no list indicating run all that are set to run by default.  If the function is
-- run, the functions run counter will be incremented, and if the resulting answer was wrong, then
-- the error count will be incremented as well. We also count those whose results aren't checked.
-- Note that in 8.2.2 it appeared that something with this was causing a segfault when running
-- threaded. I haven't tracked down the problem, and don't think it is anything I've done.

runProblemSolution :: Bool -> [Int] -> (Int, Int, Int) -> (String -> IO Int, Int, Bool)
                      -> IO (Int, Int, Int)
runProblemSolution runAll runThese (ranCount, errorCount, unchkCount) (func, probNum, runWithoutAll)
  | runAll || elem probNum runThese || (null runThese && runWithoutAll) = do
      result <- func ("Prob " ++ show probNum)
      let newRanCount   = ranCount + 1
          newErrorCount = errorCount + (result `quot` 2)
          newUnchkCount = unchkCount + (if result == 1 then 1 else 0)
          resultTuple = newRanCount `seq` newErrorCount `seq` newUnchkCount `seq`
                        (newRanCount, newErrorCount, newUnchkCount)
      return resultTuple
  | otherwise = return (ranCount, errorCount, unchkCount)

-- Defines the record used to hold command line options.  This is filled in and processed by the
-- commands in the getOpt package.

data CmdOptions = CmdOptions {
    optVersion :: Bool
  , optAll     :: Bool
  , optHelp    :: Bool
  } deriving Show

-- Default options when not specified.

defaultOptions :: CmdOptions
defaultOptions = CmdOptions {
    optVersion = False
  , optAll     = False
  , optHelp    = False
  }

-- This function describes all of the options.  Each entry contains the short option flags,
-- specified like -a, and the long option flags, specified like -all.  It also has a short
-- description of the option, which is used by the usageInfo function to generate a help message for
-- the program.  The third entry is a tuple that describes whether the option has no argument, an
-- optional one, or a required one.  It also has a value that is returned in a list from the getOpts
-- function.  Normally this would just be a value or enumeration, but the idea here is a little more
-- clever.  We return a function, that may have a value bound into it, such as the one for help.
-- This function is monadic, and takes and returns an option record.  The idea is that when the
-- getOpts function returns a list of these functions, then they can be processed using foldM, and
-- each will set the approprate value of the option record, which will start out with the default
-- values.  Each function will also do any appropriate error checking.

-- The function takes a parameter, which is the help text that is used with the help option.
-- Interesting because the help text is generated by calling this function with an empty string and
-- sending it to usageInfo.

cmdOptions :: String -> [OptDescr (CmdOptions -> IO CmdOptions)]
cmdOptions optionDescription =
  [ Option ['v']      ["version"]
    (NoArg (\_ -> do
                  progName <- getProgName
                  putStr progName
                  putStrLn $ ", version " ++ version
                  exitSuccess))
    "Print the program version."
  , Option ['h', 'u'] ["help", "usage"]
    (NoArg (\_ -> do
                  putStr optionDescription
                  exitSuccess))
    "Print usage and help text."
  , Option ['a'] ["all"]
    (NoArg (\opts -> return $ opts {optAll = True}))
    "Run all problems, even those turned off by default or still unsolved."
  ]

parseArgs :: IO (CmdOptions, [String])
parseArgs = do
  argv     <- getArgs
  progName <- getProgName

  -- The header for the help message is written by me, but the options are described using the
  -- usageInfo function called with the header and the command options.

  let helpHeader = mconcat ["Usage: ", progName, " [OPTION...] [probNumber...]\n",
                            "  If problem numbers are listed, only they will be run.\n",
                            "  If none are listed, all problems turned on by default will be run.\n",
                            "  Each problem run is individually timed, and those with a known\n",
                            "  answer are checked for correctness of the solution."]
      helpMessage = usageInfo helpHeader (cmdOptions "")
      optionTemplate = cmdOptions helpMessage
      (optFuncs, otherArgs, errs) = getOpt RequireOrder optionTemplate argv
      errorsFound = not $ null errs

  -- If errors were found while parsing the options, report them and then generate the help message.

  when errorsFound (ioError (userError (concat errs ++ "\n" ++ helpMessage)))

  -- Take the default options record, call each of the parameter functions returned by getOpt on it
  -- successively, and this will result in the option record with the user-specified options set.
  -- This can also be done with this line, but it seems less clear to me: optionRecord <- foldM
  -- (flip id) defaultOptions opts

  optionRecord <- foldM (\acc fn -> fn acc) defaultOptions optFuncs
  return (optionRecord, otherArgs)

main :: IO ()
main = do
  (cmdOptions', probNumbersStr) <- parseArgs

  let allDigits = foldl' (\acc xs -> acc && isNothing (find (not . isDigit) xs))
                         True probNumbersStr
  unless allDigits
    (ioError (userError "Error: Problem numbers must have only digits."))
  let probNumbers = map read probNumbersStr

  currTime <- getZonedTime
  let currTimeSimple = (reverse . snd . foldl removeFrac (False, [])) (show currTime)
      removeFrac (ignore, acc) curr
        | curr == ' ' = (False, curr : acc)
        | curr == '.' = (True, acc)
        | ignore = (True, acc)
        | otherwise = (False, curr : acc)
  putStrLn $ "Project Euler problem solutions written in Haskell: " ++ currTimeSimple
  putStrLn ""

  startTime <- getTime Realtime

  timeIOFunctionHRNoPrint (initPrimeStorage >>= putStr) "Init prime storage"
  putStrLn ""

  (functionsRun, errors, unchecked) <- foldM (runProblemSolution (optAll cmdOptions') probNumbers)
                                             (0, 0, 0) solutionFunctions

  endTime <- getTime Realtime
  let diff = computeElapsedTime startTime endTime
  printf "Functions run: %d  Errors: %d  Un-checked: %d  Total time: %0.3f sec.\n"
         functionsRun errors unchecked diff

-- This list contains each of the problem solution functions, along with the problem number, and a
-- boolean value indicating whether this function will be run by default or only when asked to run
-- all.  The solution function is wrapped in a timing function call because there are several
-- flavors needed, and to be in a list together with the others, they have to return the same thing,
-- which in this case is IO (Int). The value returned is 0 if no solution was supplied in the
-- runner function or the answer was correct, and 1 if the answer was wrong.

solutionFunctions :: [(String -> IO Int, Int, Bool)]
solutionFunctions =
  [(tfICk prob001 233168, 1, True), (tfICk prob002 4613732, 2, True), (tfICk prob003 6857, 3, True),
   (tfICk prob004 906609, 4, True), (tfICk prob005 232792560, 5, True),
   (tfICk prob006 25164150, 6, True), (tfICk prob007 104743, 7, True),
   (tfICk prob008 23514624000, 8, True), (tfICk prob009 31875000, 9, True),
   (tfICk prob010 142913828922, 10, True), (tfICk prob011 70600674, 11, True),
   (tfICk prob012 76576500, 12, True), (tfICk prob013 5537376230, 13, True),
   (tfICk prob014 837799, 14, True), (tfICk prob015 137846528820, 15, True),
   (tfICk prob016 1366, 16, True), (tfICk prob017 21124, 17, True),
   (tfIOCk prob018 1074, 18, True), (tfICk prob019 171, 19, True),
   (tfICk prob020 648, 20, True), (tfICk prob021 31626, 21, True),
   (tfIOCk prob022 871198282, 22, True), (tfICk prob023 4179871, 23, True),
   (tfICk prob024 2783915460, 24, True), (tfICk prob025 4782, 25, True),
   (tfICk prob026 983, 26, True), (tfICk prob027 (-59231), 27, True),
   (tfICk prob028 669171001, 28, True), (tfICk prob029 9183, 29, True),
   (tfICk prob030 443839, 30, True), (tfICk prob031 73682, 31, True),
   (tfICk prob032 45228, 32, True), (tfICk prob033 100, 33, True),
   (tfICk prob034 40730, 34, True), (tfICk prob035 55, 35, True), (tfICk prob036 872187, 36, True),
   (tfICk prob037 748317, 37, True), (tfICk prob038 932718654, 38, True),
   (tfICk prob039 840, 39, True), (tfICk prob040 210, 40, True), (tfICk prob041 7652413, 41, True),
   (tfIOCk prob042 162, 42, True), (tfICk prob043 16695334890, 43, True),
   (tfICk prob044 5482660, 44, True), (tfICk prob045 1533776805, 45, True),
   (tfICk prob046 5777, 46, True), (tfICk prob047 134043, 47, True),
   (tfICk prob048 9110846700, 48, True), (tfICk prob049 296962999629, 49, True),
   (tfICk prob050 997651, 50, True), (tfICk prob051 121313, 51, True),
   (tfICk prob052 142857, 52, True), (tfICk prob053 4075, 53, True), (tfIOCk prob054 376, 54, True),
   (tfICk prob055 249, 55, True), (tfICk prob056 972, 56, True), (tfICk prob057 153, 57, True),
   (tfICk prob058 26241, 58, True), (tfIOCk prob059 107359, 59, True),
   (tfICk prob060 26033, 60, True), (tfICk prob061 28684, 61, True),
   (tfICk prob062 127035954683, 62, True), (tfICk prob063 49, 63, True),
   (tfICk prob064 1322, 64, True), (tfICk prob065 272, 65, True), (tfICk prob066 661, 66, True),
   (tfIOCk prob067 7273, 67, True), (tfICk prob068 6531031914842725, 68, True),
   (tfICk prob069 510510, 69, True), (tfICk prob070 8319823, 70, True),
   (tfICk prob071 428570, 71, True), (tfICk prob072 303963552391, 72, True),
   (tfICk prob073 7295372, 73, True), (tfICk prob074 402, 74, True),
   (tfICk prob075 161667, 75, True), (tfICk prob076 190569291, 76, True),
   (tfICk prob077 71, 77, True), (tfICk prob078 55374, 78, True),
   (tfIOCk prob079 73162890, 79, True), (tfICk prob080 40886, 80, True),
   (tfIOCk prob081 427337, 81, True), (tfIOCk prob082 260324, 82, True),
   (tfIOCk prob083 425185, 83, True), (tfICk prob084 101524, 84, True),
   (tfICk prob085 2772, 85, True), (tfICk prob086 1818, 86, True),
   (tfICk prob087 1097343, 87, True), (tfICk prob088 7587457, 88, True),
   (tfIOCk prob089 743, 89, True), (tfICk prob090 1217, 90, True),
   (tfICk prob091 14234, 91, True), (tfICk prob092 8581146, 92, True),
   (tfICk prob093 1258, 93, True), (tfICk prob094 518408346, 94, True),
   (tfICk prob095 14316, 95, True), (tfIOCk prob096 24702, 96, True),
   (tfICk prob097 8739992577, 97, True), (tfIOCk prob098 18769, 98, True),
   (tfIOCk prob099 709, 99, True), (tfICk prob100 756872327473, 100, True)
  ]
