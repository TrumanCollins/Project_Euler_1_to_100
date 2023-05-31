-- Project Euler utility programs.
-- By Truman Collins
-- April 19, 2014 to ...

-- The following packages are needed to compile this:
-- cabal install containers
-- cabal install vector

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module EulerUtil (
  myFibNumbers,
  myFibNumbersInteger,
  triangleNumbers,
  triangleNumbers64,
  squareNumbers,
  squareNumbers64,
  pentagonalNumbers,
  hexagonalNumbers,
  heptagonalNumbers,
  octagonalNumbers,
  pascalsTriangle,
  pascalsTriangleG,
  stirling1stKind,
  stirling2ndKind,
  foldlUsingfoldr,
  dfsGeneric,
  bfsGeneric,
  fibBig,
  fibBigMod,
  ptptWithinLimitPerimeter,
  ptptWithinLimitCLength,
  ptptWithinLimitabLength,
  ptptNoLimit,
  pythagoreanTriplesPrimTree,
  pythagoreanTriplesPrimTreeOnlyPrimitives,
  pythagoreanTriplesPrimTreeOnlyPrimitivesInt,
  pythagoreanTriplesPrimTreeOnlyPrimitivesInt64,
  pythExtend,
  PrimOrNot(..),
  LimitType(..),
  pythagoreanTriplesDickson,
  pythagoreanTriplesInt,
  pythagoreanTriplesInt64,
  pythagoreanTriples,
  pythagoreanTriplesA,
  modularExponentiation,
  isqrt,
  isqrt64,
  isqrtInt,
  isqrtG,
  modularMultInverseM,
  modularMultInverse,
  solveMinLinCongruenceChineseRemainder,
  extGCD,
  numberInHex,
  numberInBinary,
  numberInBase,
  integerInBinary,
  intToOptimalRomanNumerals,
  romanNumeralsToNumber,
  convertByteStringsToInts,
  listOfProperFactorSumsToLimit,
  listOfProperFactorSumsToLimit64,
  arrayOfProperFactorSumsToLimit,
  arrayOfProperFactorSumsToLimitOther,
  allPartitions,
  all2Partitions,
  allPartitionsInitial,
  partitionsOfLength,
  continuedFractionNSteps,
  successiveTermsOfContinuedFraction,
  continuedSqrtFrac,
  continuousFractionRepresentationOfE,
  pellsEquationSolutions,
  pellsEquationNegOneSolutions,
  printAllArithPerms,
  allArithPerms,
  removeDups,
  removeDupsInOrder,
  removeDupsInOrderFoldr,
  removeDupsInOrderFoldr2,
  removeDupsGroup,
  countDups,
  countDupsFoldr,
  removeElementsSorted,
--  removeSecondList,
  intersectionSorted,
  initLast,
  subsets,
  subsetsOfSize,
  disjointSubsetPairsOfSize,
  subsetsProd,
  subsetsProdPF,
  divisors,
  pairsOrdered,
  countUniqueInSortedList,
  permsLexicographic,
  allPairsFirstAndRest,
  allWays,
  spreadList,
  spreadListSkip,
  spreadOutList,
  splitList,
  breakRangeIntoChunks,
  splitListFn,
  allEqual,
  stringToInt,
  intToListOfInts,
  int64ToListOfInts,
  integerToListOfInts,
  intToListOfIntsBase,
  integerToListOfIntsBase,
  intListToInt,
  intListToInt64,
  intListToInteger,
  intListToIntegerBase,
  permutedPair,
  permutedPairNoMod,
  permutedPair1,
  permutedPairNoMod1,
  permsWithDups,
  palindromic,
  isPandigital,
  isPandigitalBase,
  isSuperPandigital,
  parseWordsFromString,
  parseIntsFromString,
  prodNgtM,
  fact,
  comb,
  perm,
  risingFactPower,
  fallingFactPower,
  powIntegerIntNoWarn,
  powInt64IntNoWarn,
  RepeatsRecipT,
  repeatsInReciprocal,
  isFirstOrderPat,
  genFirstOrderSeq,
  isSecondOrderPat,
  genSecondOrderSeq,
  isThirdOrderPat,
  genThirdOrderSeq,
  isFourthOrderPat,
  genFourthOrderSeq,
  MinMaxT(..),
  findMinOrMax,
  sumWithRem,
  prodWithRem,

  -- Functions and types for generating state machines and using them.

  StateMap,
  genStateMap,
  StateVec,
  genStateVec,
  genStateVecFromStateMap,
  StateAndCount (dfaState_r, dfaCount_r),
  isAcceptState,
  StateSet,
  statesAndCountsStartingAtState0,
  sumFinalCountsAfterNSteps
) where

import Data.Char
import Data.Int
import Data.Bits
import Data.Ratio
import Data.List
import Data.Maybe
import Data.Function
import Data.Array.Unboxed as UA
import qualified Data.Bifunctor as BF
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BC
import Control.Parallel.Strategies
import Numeric
import PrimeMod

-- Various lists of integer sequences.

myFibNumbers :: [Int]
myFibNumbers = 0 : 1 : zipWith (+) myFibNumbers (tail myFibNumbers)

myFibNumbersInteger :: [Integer]
myFibNumbersInteger = 0 : 1 : zipWith (+) myFibNumbersInteger
                                           (tail myFibNumbersInteger)

triangleNumbers :: [Int]
triangleNumbers = scanl1 (+) [1..]

triangleNumbers64 :: [Int64]
triangleNumbers64 = scanl1 (+) [1..]

squareNumbers :: [Int]
squareNumbers = scanl1 (+) [1,3..]

squareNumbers64 :: [Int64]
squareNumbers64 = scanl1 (+) [1,3..]

pentagonalNumbers :: [Int]
pentagonalNumbers = scanl1 (+) [1,4..]

hexagonalNumbers :: [Int]
hexagonalNumbers = scanl1 (+) [1,5..]

heptagonalNumbers :: [Int]
heptagonalNumbers = scanl1 (+) [1,6..]

octagonalNumbers :: [Int]
octagonalNumbers = scanl1 (+) [1,7..]

-- Generates an infinite list of Pascal's Triangle rows.

pascalsTriangle :: [[Int]]
pascalsTriangle = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]

-- Generic version.

pascalsTriangleG :: (Integral a) => [[a]]
pascalsTriangleG = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]

-- Generates an infinite list of Stirling Numbers of the First Kind rows for k = 1 to n.

stirling1stKind :: [[Integer]]
stirling1stKind = map snd (iterate genNextRow (1, [1]))
  where
    genNextRow (n, row) = (n + 1, zipWith (combineLastRowEles n) (row ++ [0]) (0 : row))
    combineLastRowEles lastN lastK lastKMinus1 = lastN * lastK + lastKMinus1

-- Generates an infinite list of Stirling Numbers of the Second Kind rows for k = 1 to n.

stirling2ndKind :: [[Integer]]
stirling2ndKind = iterate (\row -> zipWith3 combineLastRowElesAndK [1..] (row ++ [0]) (0 : row)) [1]
  where combineLastRowElesAndK k lastK lastKMinus1 = k * lastK + lastKMinus1

-- Here is foldl implemented using foldr, where foldr accumulates a stack of functions and then they
-- are evaluated. It's possible to modify the behavior of foldl using this technique. For example,
-- it is possible to short-circuit foldl in this way. See https://wiki.haskell.org/Foldl_as_foldr
-- See also removeDupsInOrderFoldr in this file and another description in prob117.

foldlUsingfoldr :: Foldable t1 => (t2 -> t3 -> t2) -> t2 -> t1 t3 -> t2
foldlUsingfoldr stepL zeroL xs = foldr stepR id xs zeroL
  where stepR lastL accR accInitL = accR (stepL accInitL lastL)

-- Depth-first and Breadth-first search functions.  All that is needed is the beginning state, and a
-- successor function that returns the list of next states given a state. These functions will not
-- handle cycles in the search space.
-- If the client is going to search the entire tree, then bfs will be give faster performance, but
-- will use more memory, while dfs will use less memory but be slower.

dfsGeneric, bfsGeneric :: (a -> [a]) -> a -> [a]
dfsGeneric succFn start = start : concatMap (dfsGeneric succFn) (succFn start)
bfsGeneric succFn start = concat $ takeWhile (not . null) $ iterate (concatMap succFn) [start]

-- Generate very large Fibonacci numbers using a method devised by Daisuke Takahashi.

fibBig :: Integer -> Integer
fibBig n
  | odd n = v * f + ((n .&. 0x2) - 1)
  | otherwise = u * f
  where
    m = n `quot` 2
    (u, v) = fib_inner m
    f = 2 * v - u

    fib_inner :: Integer -> (Integer, Integer)
    fib_inner n'
      | n' == 0 = (0, 1)
      | odd n' = let fstVal = v1 + u1
                     sndVal = fstVal `seq` 3 * v1 - 2 * (u1 - q)
                 in sndVal `seq` (fstVal, sndVal)
      | otherwise = let fstVal = 2 * (v1 + q) - 3 * u1
                        sndVal = fstVal `seq` u1 + v1
                    in sndVal `seq` (fstVal, sndVal)
      where
        m' = n' `quot` 2
        (u', v') = fib_inner m'
        q = (n' .&. 0x2) - 1
        u1 = u' * u'
        v1 = v' * v'

-- Generate very large Fibonacci numbers using a method devised by Daisuke Takahashi.
-- Use a mod value for the result and as calculation progresses.

fibBigMod :: Integer -> Integer -> Integer
fibBigMod modVal n
  | odd n = (v * f + ((n .&. 0x2) - 1)) `mod` modVal
  | otherwise = (u * f) `mod` modVal
  where
    m = n `quot` 2
    (u, v) = fib_inner m
    f = 2 * v - u

    fib_inner :: Integer -> (Integer, Integer)
    fib_inner n'
      | n' == 0 = (0, 1)
      | odd n' = let fstVal = (v1 + u1) `rem` modVal
                     sndVal = fstVal `seq` (3 * v1 - 2 * (u1 - q)) `mod` modVal
                 in sndVal `seq` (fstVal, sndVal)
      | otherwise = let fstVal = (2 * (v1 + q) - 3 * u1) `mod` modVal
                        sndVal = fstVal `seq` (u1 + v1) `rem` modVal
                    in sndVal `seq` (fstVal, sndVal)
      where
        m' = n' `quot` 2
        (u', v') = fib_inner m'
        q = (n' .&. 0x2) - 1
        u1 = u' * u'
        v1 = v' * v'

-- There are three different algorithms here for generating Pythagorean triples,
-- each with different characteristics.
--
-- pythagoreanTriplesPrimTree generates Pythagorean Triples by using a matrix
--   generative method for generating primitive triples, and then multiplies
--   these by successively larger integers to get non-primitive triples.
--   This technique takes a limiting function so that it will only generate
--   a finite list of triples.  Also, they will not come out in a nice order.
-- pythagoreanTriplesDickson is vastly faster than the others, and generates
--   triples roughly as 'a' increases, although there is a lot of variability
--   here.  The time taken is linear.
-- pythagoreanTriplesInt generates them ordered by the value of 'c'.  The
--   time taken is nlogn or perhaps worse.
-- pythagoreanTriples generates them ordered by 'a', but is quite slow.
-- pythagoreanTriplesA is an optimized version of pythagoreanTriples.

-- Three useful limit functions to use while generating Pythagorean Triples
-- with pythagoreanTriplesPrimTree.

ptptWithinLimitPerimeter :: (Num a, Ord a) => a -> (a, a, a) -> Bool
ptptWithinLimitPerimeter perim (a,b,c) = a+b+c <= perim
ptptWithinLimitCLength :: Ord a1 => a1 -> (a2, b, a1) -> Bool
ptptWithinLimitCLength len (_,_,c) = c <= len
ptptWithinLimitabLength :: Ord a => a -> (a, a, c) -> Bool
ptptWithinLimitabLength len (a,b,_) = a <= len && b <= len
ptptNoLimit :: (a, b, c) -> Bool
ptptNoLimit (_, _, _) = True

-- Use the tree generation method for primitive triples, then expand on each of these generating
-- non-primitive triples until we hit the limit.

pythagoreanTriplesPrimTree :: ((Int,Int,Int) -> Bool) -> [(Int,Int,Int)]
pythagoreanTriplesPrimTree limitFunc = ptpt [(3,4,5)]
  where
    nonPrimitives :: Int -> (Int,Int,Int) -> [(Int,Int,Int)] -> [(Int,Int,Int)]
    nonPrimitives factor (a,b,c) nextPrimitives
      | limitFunc (newA, newB, newC)
        = (newA,newB,newC) : nonPrimitives (factor + 1) (a,b,c) nextPrimitives
      | otherwise = ptpt nextPrimitives
      where
        !newA = factor * a
        !newB = factor * b
        !newC = factor * c
    ptpt :: [(Int,Int,Int)] -> [(Int,Int,Int)]
    ptpt [] = []
    ptpt ((a,b,c):pts)
      | limitFunc (a,b,c) = (a,b,c) : nonPrimitives 2 (a,b,c) (pythExtend a b c pts)
      | otherwise = ptpt pts

-- Use the tree generation method for primitive triples, and only return these.

pythagoreanTriplesPrimTreeOnlyPrimitives :: ((Integer,Integer,Integer) -> Bool)
                                            -> [(Integer,Integer,Integer)]
pythagoreanTriplesPrimTreeOnlyPrimitives limitFunc = ptpt [(3,4,5)]
  where
    ptpt :: [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
    ptpt [] = []
    ptpt ((a,b,c):pts)
      | limitFunc (a,b,c) = (a,b,c) : ptpt (pythExtend a b c pts)
      | otherwise = ptpt pts

pythagoreanTriplesPrimTreeOnlyPrimitivesInt :: ((Int, Int, Int) -> Bool)
                                            -> [(Int, Int, Int)]
pythagoreanTriplesPrimTreeOnlyPrimitivesInt limitFunc = ptpt [(3, 4, 5)]
  where
    ptpt :: [(Int, Int, Int)] -> [(Int, Int, Int)]
    ptpt [] = []
    ptpt ((a,b,c):pts)
      | limitFunc (a,b,c) = (a,b,c) : ptpt (pythExtend a b c pts)
      | otherwise = ptpt pts

pythagoreanTriplesPrimTreeOnlyPrimitivesInt64 :: ((Int64, Int64, Int64) -> Bool)
                                            -> [(Int64, Int64, Int64)]
pythagoreanTriplesPrimTreeOnlyPrimitivesInt64 limitFunc = ptpt [(3, 4, 5)]
  where
    ptpt :: [(Int64, Int64, Int64)] -> [(Int64, Int64, Int64)]
    ptpt [] = []
    ptpt ((a,b,c):pts)
      | limitFunc (a,b,c) = (a,b,c) : ptpt (pythExtend a b c pts)
      | otherwise = ptpt pts

pythExtend :: (Integral a) => a -> a -> a -> [(a, a, a)] -> [(a, a, a)]
pythExtend a b c pts = nextPrimA : nextPrimB : nextPrimC : pts
  where
    nextPrimA = (a - 2*b + 2*c, 2*a - b + 2*c, 2*a - 2*b + 3*c)
    nextPrimB = (a + 2*b + 2*c, 2*a + b + 2*c, 2*a + 2*b + 3*c)
    nextPrimC = ((-a) + 2*b + 2*c, (-2)*a + b + 2*c, (-2)*a + 2*b + 3*c)

-- Use the formula by Dickson, for all even positive integers r, s, and t, r^2 = 2st, a = r + s,
-- b = r + t, and c = r + s + t, will generate all Pythagorean triples.  To generate s and t
-- efficiently, we reformulate to: (r*r)/2 = st, and since r is even: find the prime factors of
-- (r/2), double the factors, and then multiply this by 2, which gives us the prime factors of st.
-- We can then use these to generate all integer values of s and t.  This function takes a limit,
-- and limit type along with whether to only generate primitive triples or all of them.

data PrimOrNot = OnlyPrimitive | AllTriples deriving Eq
data LimitType = CLength | Perimeter | AAndBLength deriving Eq

pythagoreanTriplesDickson :: PrimOrNot -> (LimitType, Int64) -> ((Int64, Int64, Int64) -> Bool)
                          -> [(Int64, Int64, Int64)]
pythagoreanTriplesDickson primOrNot (kindOfLimit, limit) saveFn = allResultsUpToLimit
  where

    -- Generate all triples up to the limit. Note that the full list is entirely generated before
    -- being returned.

    allResultsUpToLimit = primeFactorWalk maxRNeeded [] [] (accumTriples triplePassesLimitFn)
                                          dfsPrimeToPFs True id

    -- This is the accumulator function sent to the prime walk through function.  For each number
    -- and associated factors, it will generate all related pythagorean triples using the Dickson
    -- method.

    accumTriples :: ((Int64, Int64, Int64) -> Bool) -> [(Int64, Int64, Int64)] -> Int64
                    -> [(Int, PrimeT)] -> [(Int64, Int64, Int64)]
    accumTriples passesLimitFn acc r factors
      | primOrNot == AllTriples = foldl' accTripleAndMultiples acc allSAndTPairs
      | otherwise = foldl' (\acc' st -> let !newTrip = tripleFromSAndT st
                                        in if saveFn newTrip then newTrip : acc' else acc')
                           acc allSAndTPairs
      where
        allSAndTPairs = genAllSAndTPairs (1, 1) sqFactorsByPrime []

        accTripleAndMultiples acc' st = accTriplesUntilTooBig acc' triple
          where
            !triple@(a, b, c) = tripleFromSAndT st
            accTriplesUntilTooBig !acc'' triple'@(a', b', c')
              | not (passesLimitFn triple') = acc''
              | otherwise = let !newA = a' + a; !newB = b' + b; !newC = c' + c
                                !newTriple = (newA, newB, newC)
                                !newAcc = if saveFn triple' then triple' : acc'' else acc''
                            in accTriplesUntilTooBig newAcc newTriple
        tripleFromSAndT (s, t) = let !a = r64 + s; !b = r64 + t; !c = r64 + s + t in (a, b, c)

        -- Given a pair representing the current s and t values, and a list of prime factor powers,
        -- generate all co-prime s and t pairs such that s < t.

        genAllSAndTPairs (s, t) [] acc' = if s < t then (s, t) : acc' else acc'
        genAllSAndTPairs (s, t) (f : fs) acc' = returnAcc
          where accFWithS = let !newS = s * f in genAllSAndTPairs (newS, t) fs acc'
                returnAcc = let !newT = t * f in genAllSAndTPairs (s, newT) fs accFWithS
        sqFactorsByPrime = map genFactorExp64 factors

        -- Given a prime number and a count generate p^(2c) and p^(2*c-1) if p == 2.  Convert to
        -- Int64 too.

        genFactorExp64 :: (Int, PrimeT) -> Int64
        genFactorExp64 (c, p)
          | p == 2 = 2 ^ (fromIntegral (c + c - 1) :: Int64)
          | otherwise = let p64 = fromIntegral p
                        in p64 ^ (fromIntegral (c + c) :: Int64)
        r64 = fromIntegral r :: Int64

    -- Determine the maximum r value we need to look at to make sure to find all of the triples in
    -- the wanted range. The limit will be different based on the type of limit.
    -- Also determine the function to test whether a triple is within the limit.

    (maxRNeeded, triplePassesLimitFn)
      | kindOfLimit == CLength     = (floor (limit_d / (1 + sqrt2)),
                                      \(_, _, c) -> c <= limit)
      | kindOfLimit == Perimeter   = (floor (limit_d / (3 + 2 * sqrt2)),
                                      \(a, b, c) -> (a+b+c) <= limit)
      | kindOfLimit == AAndBLength = (floor (limit_d / (1 + 1 / sqrt2)),
                                      \(a, b, _) -> a <= limit && b <= limit)
      | otherwise = undefined
      where sqrt2 = 1.414213562373095 :: Double; limit_d = fromIntegral limit :: Double

-- Generate Pythagorean triples where all three values are integers.  This is an infinite list
-- generated ordered by the value of 'c'.  The idea is to walk 'c' through the values of squares,
-- and walk 'b' through the squares from the first over 'c'/2 up to 'c'.  These will be valid
-- Pythagorean triples when 'c'-'b' is a square.  As the value of 'b' is increased, insert each 'b'
-- and its square into a map that we can consult for valid square values of 'a'.  There is a version
-- of this function for Int and for Int64

pythagoreanTriplesInt :: [(Int, Int, Int)]
pythagoreanTriplesInt = pTriples (drop 4 squaresWithRoot) bStart bStart initialMap
  where
    initialMap :: M.Map Int Int
    initialMap = M.singleton 9 3
    squaresWithRoot :: [(Int, Int)]
    squaresWithRoot = zip squareNumbers [1..]
    bStart = drop 3 squaresWithRoot
    pTriples :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> M.Map Int Int -> [(Int, Int, Int)]
    pTriples [] _ _ _ = []
    pTriples _ [] _ _ = []
    pTriples _ _ [] _ = []
    pTriples ((cSq,c):cs) ((bSq,b):bs) ((bbSq,bb):bbs) priorMap
      | c == b = if fst (head cs) > shiftL bbSq 1
                 then pTriples cs bbs bbs (M.insert bbSq bb priorMap)
                 else pTriples cs ((bbSq,bb):bbs) ((bbSq,bb):bbs) priorMap
      | isNothing aSqLookup = pTriples ((cSq,c):cs) bs ((bbSq,bb):bbs) priorMap
      | otherwise = (fromJust aSqLookup, b, c)
                      : pTriples ((cSq,c):cs) bs ((bbSq,bb):bbs) priorMap
      where
        aSqLookup = if aSqTest .&. 3 == 2 then Nothing else M.lookup aSqTest priorMap
        aSqTest = cSq - bSq

pythagoreanTriplesInt64 :: [(Int, Int, Int)]
pythagoreanTriplesInt64 = pTriples (drop 4 squaresWithRoot) bStart bStart initialMap
  where
    initialMap :: M.Map Int64 Int
    initialMap = M.singleton 9 3
    squaresWithRoot :: [(Int64, Int)]
    squaresWithRoot = zip squareNumbers64 [1..]
    bStart = drop 3 squaresWithRoot
    pTriples :: [(Int64, Int)] -> [(Int64, Int)] -> [(Int64, Int)] -> M.Map Int64 Int
                -> [(Int, Int, Int)]
    pTriples [] _ _ _ = []
    pTriples _ [] _ _ = []
    pTriples _ _ [] _ = []
    pTriples ((cSq, c) : cs) ((bSq, b) : bs) ((bbSq, bb) : bbs) priorMap
      | c == b = if fst (head cs) > shiftL bbSq 1
                 then pTriples cs bbs bbs (M.insert bbSq bb priorMap)
                 else pTriples cs ((bbSq,bb):bbs) ((bbSq,bb):bbs) priorMap
      | isNothing aSqLookup = pTriples ((cSq,c):cs) bs ((bbSq,bb):bbs) priorMap
      | otherwise = (fromJust aSqLookup, b, c)
                      : pTriples ((cSq,c):cs) bs ((bbSq,bb):bbs) priorMap
      where
        aSqLookup = if aSqTest .&. 3 == 2 then Nothing else M.lookup aSqTest priorMap
        aSqTest = cSq - bSq

-- Generate an infinite list of Pythagorean triples.
-- Minimize the number of c values to look at.  Don't do any square roots.
-- Minimize the number of parameters passed.  Even though we 
-- do more square operations, this seems to be a win for performance.

pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = pTriples 2 3 4
  where
    pTriples :: Int -> Int -> Int -> [(Int, Int, Int)]
    pTriples a b c
      | c*c > a*a + b*b = pTriples a (b+1) c
      | c*c == a*a + b*b = (a,b,c) : pTriples a (b+1) (c+1)
      | (b+1)*(b+1) - b*b > a*a = pTriples (a+1) (a+2) (a+3)
      | otherwise = pTriples a b (c+1)

-- Generate an infinite list of Pythagorean triples.  Minimize the number of c values to look at.
-- Don't do any square roots.  This vesion tries to minimize the number of squares that are done at
-- the expence of passing more parameters.  As it turns out, this doesn't appear to be a win.
        
pythagoreanTriplesA :: [(Int, Int, Int)]
pythagoreanTriplesA = pTriples 2 4 3 9 16 4
  where
    pTriples :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int, Int)]
    pTriples a aSq b bSq bPlus1Sq c
      | c*c > aSq + bSq = pTriples a aSq (b+1) bPlus1Sq ((b + 2)*(b + 2)) c
      | c*c == aSq + bSq = (a,b,c) : pTriples a aSq (b+1) bPlus1Sq ((b + 2)*(b + 2)) (c+1)
      | bPlus1Sq - bSq > aSq = pTriples (a+1) ((a+1)*(a+1)) (a+2) ((a+2)*(a+2)) ((a+3)*(a+3)) (a+3)
      | otherwise = pTriples a aSq b bSq bPlus1Sq (c+1)

-- Fast modular exponentiation.
-- Use a method where we square the base each loop, and multiply it into the result for each bit set
-- in the exponent.

modularExponentiation :: (Integral a, Bits a) => a -> a -> a -> a
modularExponentiation base expon modulus = modular_exp 1 (base `rem` modulus) expon
  where
    modular_exp currResult currBase currExp
      | currExp == 0 = currResult
      | currExp .&. 1 == 0 = modular_exp currResult newBase newExp
      | otherwise = let newResult = (currResult * currBase) `rem` modulus
                    in newResult `seq` modular_exp newResult newBase newExp
      where
        newExp = shiftR currExp 1
        newBase = (currBase * currBase) `rem` modulus

-- Compute the integer square root iteratively. Use Integer so we don't overflow with n^2.

isqrt :: Integer -> Integer
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n)
               $ iterate (\x -> (x + n `quot` x) `quot` 2) (n `quot` 2)
isqrt64 :: Int64 -> Int64
isqrt64 n = fromIntegral $ isqrt (fromIntegral n)
isqrtInt :: Int -> Int
isqrtInt n = fromIntegral $ isqrt (fromIntegral n)

isqrtG :: (Integral a) => a -> a
isqrtG 0 = 0
isqrtG 1 = 1
isqrtG n = head $ dropWhile (\x -> x*x > n)
                $ iterate (\x -> (x + n `quot` x) `quot` 2) (n `quot` 2)

-- Returns the modular multiplicative inverse given a value and a modulus. If there is not a single
-- valid one, then Nothing is returned. The idea here is that to use division while keeping the
-- result of a computation (mod m), it's easy using multiplications, addition, and subtraction and
-- then just do a (mod m) after, but for division, it is harder. If you can find the multiplicative
-- inverse of a divisor given the modulus, then you can multiply by that to get the result.
-- Always return a non-negative value, although extGCD will also return negative ones.

modularMultInverseM :: (Integral a) => a -> a -> Maybe a
modularMultInverseM modVal val
  | gcdVal == 1 = let nonNegT1 = if t1 < 0 then t1 + modVal else t1 in nonNegT1 `seq` Just nonNegT1
  | otherwise = Nothing
  where (t1, _, gcdVal) = extGCD val modVal

-- This version returns a value if there is a unique one, and otherwise prints an error using the
-- string passed in.

modularMultInverse :: (Integral a, Show a) => String -> a -> a -> a
modularMultInverse caller modVal val
  | isJust modMultInvM = fromJust modMultInvM
  | otherwise = error $ "Non-unique multiplicative inverse called from " ++ caller
                        ++ " Value: " ++ show val ++ " modVal: " ++ show modVal
  where modMultInvM = modularMultInverseM modVal val

-- Compute the extended GCD of two integers, where the result is (t1, t2, gcd)
-- For example: extGCD 24 9 = (-1, 3, 3), so (gcd 24 9) == 3, and
--     -1 * 24 + 3 * 9 = 3

extGCD :: (Integral a) => a -> a -> (a, a, a)
extGCD a 0 = (1, 0, a)
extGCD a b = let (q, r) = a `divMod` b
                 (s, t1, g) = extGCD b r
                 t2 = s - q * t1
             in t2 `seq` (t1, t2, abs g)

-- Given a set of modular congruences, solve for the smallest value satisfying them all.  This is
-- used to find the least answer for the Chinese Remainder Theorem. For example: x = 2 mod 3; x = 3
-- mod 5; and x = 2 mod 7, call this function with [2, 3, 2] [3, 5, 7], and it will return the
-- smallest x that is consistent with all these equations, which is 23 in this case. Note that the
-- residues need to all be smaller than the corresponding modVals, and the modVals need to all be
-- relatively prime. See page 68 of "Number Theory" by George Andrews for the technique.

solveMinLinCongruenceChineseRemainder :: (Integral a) => [a] -> [a] -> Maybe a
solveMinLinCongruenceChineseRemainder residues modVals
  | any isNothing nCompValsM = Nothing
  | otherwise = Just result
  where
    result = ((`mod` fullProduct) . sum) (zipWith3 (\a b c -> a * b * c) residues nVals nCompVals)
    nVals  = map (div fullProduct) modVals
    fullProduct = product modVals
    nCompValsM  = zipWith modularMultInverseM modVals nVals
    nCompVals   = map fromJust nCompValsM

-- Return a string representing the given integer represented in hex, binary, and arbitrary.

numberInHex :: (Show a, Integral a) => a -> String
numberInHex n = showHex n ""
numberInBinary :: (Show a, Integral a) => a -> String
numberInBinary n = showIntAtBase 2 intToDigit n ""
numberInBase :: (Show a, Integral a) => a -> a -> String
numberInBase b n = showIntAtBase b intToDigit n ""

-- Convert an integer to a string showing its binary representation.

integerInBinary :: Integer -> String
integerInBinary n
  | n == 0 = "0"
  | n < 0 = integerInBinary (-n)
  | otherwise = integerBinaryIter "" n
  where
    integerBinaryIter acc n'
      | n' == 0 = acc
      | rightmostBit == 1 = integerBinaryIter ('1' : acc) shiftedN
      | otherwise = integerBinaryIter ('0' : acc) shiftedN
      where
        rightmostBit = n' .&. 0x1
        shiftedN = shiftR n' 1

-- Converts an int to roman numerals.

intToOptimalRomanNumerals :: Int -> String
intToOptimalRomanNumerals 0 = []
intToOptimalRomanNumerals num = n2OptRomNum num numerals
  where
    numerals = [('M', 1000, 900, 1), ('D', 500, 400, 0), ('C', 100, 90, 1), ('L', 50, 40, 0),
                ('X', 10, 9, 1), ('V', 5, 4, 0), ('I', 1, 1, 0)]
    n2OptRomNum _ [] = []
    n2OptRomNum 0 _ = []
    n2OptRomNum n currRNums@((rDig, rVal, rSubVal, rSkip) : xs)
      | n < rSubVal = n2OptRomNum n xs
      | n < rVal = n2OptRomNum nxtVal nextListToUse ++ n2OptRomNum (n + nxtVal) currRNums
      | otherwise = rDig : n2OptRomNum (n - rVal) currRNums
      where
        nextListToUse = drop rSkip xs
        (_, nxtVal, _, _) = head nextListToUse

-- Convert a roman numeral to an int.

romanNumeralsToNumber :: String -> Int
romanNumeralsToNumber "" = 0
romanNumeralsToNumber str = rn2Num str (tail str) 0
  where
    rnValues :: UV.Vector Int
    rnValues = UV.accum (+) (UV.replicate 90 0) [(ord 'M', 1000), (ord 'D', 500),
                            (ord 'C', 100), (ord 'L', 50), (ord 'X', 10),
                            (ord 'V', 5), (ord 'I', 1)]
    rn2Num :: String -> String -> Int -> Int
    rn2Num [] _ acc = acc
    rn2Num (f : fs) seconds acc
      | null seconds || firstVal >= secondVal = rn2Num fs ss (acc + firstVal)
      | otherwise = rn2Num fs ss (acc - firstVal)
      where
        (s : ss) = seconds
        firstVal = rnValues UV.! ord f
        secondVal = rnValues UV.! ord s

-- Convert a list of ints represented as bytestrings to a list of ints.  Does no error checking.

convertByteStringsToInts :: [BC.ByteString] -> [Int]
convertByteStringsToInts = map (fst . fromJust . BC.readInt)

-- Generates a list of pairs of numbers up to the given limit along with the sum of that number's
-- proper factors.  The list will not be in order.

listOfProperFactorSumsToLimit :: Int -> [(Int, Int)]
listOfProperFactorSumsToLimit limit = listProperFactorSumPairs
  where
    listProperFactorSumPairs = genListOfProperFactors [(1,0)] 1 1 myPrimes
                               [1] (Set.insert 1 Set.empty)
    genListOfProperFactors :: [(Int,Int)] -> Int -> Int -> [PrimeT]
                              -> [Int] -> Set.Set Int -> [(Int,Int)]
    genListOfProperFactors _ _ _ [] _ _ = []
    genListOfProperFactors propFactorSums currProd currSum
                           (currPrime : remainderPrimeList)
                           currFactors usedFactors
      -- If multiplying the passed in product by the current prime results in a
      -- number over the limit, then we have nothing more to do, as neither this
      -- prime or any larger than it are usable to add to the current set of
      -- factors.
      | nextProd > limit = propFactorSums
      | otherwise = newPropFactorSums
      where
        nextProd = currProd * fromIntegral currPrime
        -- Add the next prime without one more of these.
        nextPropFactorSums = genListOfProperFactors propFactorSums
                             currProd currSum remainderPrimeList currFactors
                             usedFactors
        -- Now recursively add this prime to the mix.  Add the sum computed for
        -- the next product, which is the new sum minus the next product, because
        -- we want the proper factors.
        newPropFactorSums  = genListOfProperFactors
                             ((nextProd, newSum - nextProd) : nextPropFactorSums)
                             nextProd newSum (currPrime : remainderPrimeList)
                             newFactors newUsedFactors
        (newSum, newFactors, newUsedFactors)
            = foldl' primeAndFactor (currSum, currFactors, usedFactors) currFactors
        primeAndFactor :: (Int, [Int], Set.Set Int) -> Int -> (Int, [Int], Set.Set Int)
        primeAndFactor (currSum', currFactors', usedFactors') factor =
          if currSum `seq` Set.member potentialFactor usedFactors'
          then (currSum', currFactors', usedFactors')
          else (currSum' + potentialFactor, potentialFactor : currFactors',
                Set.insert potentialFactor usedFactors')
          where
            !potentialFactor = factor * fromIntegral currPrime

-- Generates a list of pairs of numbers up to the given limit along with the sum of that number's
-- proper factors.  The list will not be in order. The 64-bit version is needed for problem 95
-- on 32-bit machines.

listOfProperFactorSumsToLimit64 :: Int64 -> [(Int64, Int64)]
listOfProperFactorSumsToLimit64 limit = listProperFactorSumPairs
  where
    listProperFactorSumPairs = genListOfProperFactors [(1,0)] 1 1 myPrimes
                               [1] (Set.insert 1 Set.empty)
    genListOfProperFactors :: [(Int64, Int64)] -> Int64 -> Int64 -> [PrimeT]
                              -> [Int64] -> Set.Set Int64 -> [(Int64, Int64)]
    genListOfProperFactors _ _ _ [] _ _ = []
    genListOfProperFactors propFactorSums currProd currSum
                           (currPrime : remainderPrimeList)
                           currFactors usedFactors
      -- If multiplying the passed in product by the current prime results in a
      -- number over the limit, then we have nothing more to do, as neither this
      -- prime or any larger than it are usable to add to the current set of
      -- factors.
      | nextProd > limit = propFactorSums
      | otherwise = newPropFactorSums
      where
        nextProd = currProd * fromIntegral currPrime
        -- Add the next prime without one more of these.
        nextPropFactorSums = genListOfProperFactors propFactorSums
                             currProd currSum remainderPrimeList currFactors
                             usedFactors
        -- Now recursively add this prime to the mix.  Add the sum computed for
        -- the next product, which is the new sum minus the next product, because
        -- we want the proper factors.
        newPropFactorSums  = genListOfProperFactors
                             ((nextProd, newSum - nextProd) : nextPropFactorSums)
                             nextProd newSum (currPrime : remainderPrimeList)
                             newFactors newUsedFactors
        (newSum, newFactors, newUsedFactors)
            = foldl' primeAndFactor (currSum, currFactors, usedFactors) currFactors
        primeAndFactor :: (Int64, [Int64], Set.Set Int64) -> Int64
                          -> (Int64, [Int64], Set.Set Int64)
        primeAndFactor (currSum', currFactors', usedFactors') factor =
          if currSum' `seq` Set.member potentialFactor usedFactors'
          then (currSum', currFactors', usedFactors')
          else (currSum' + potentialFactor, potentialFactor : currFactors',
                Set.insert potentialFactor usedFactors')
          where
            !potentialFactor = factor * fromIntegral currPrime

-- Returns an array whose index is the integers from 1 up to limit and
-- whose contents is the sum of the number's proper factors.

arrayOfProperFactorSumsToLimit :: Int -> UArray Int Int
arrayOfProperFactorSumsToLimit limit
  = UA.array (1, limit) (listOfProperFactorSumsToLimit limit)

-- This version is much slower, but more straight-forward.

arrayOfProperFactorSumsToLimitOther :: Int -> UArray Int Int
arrayOfProperFactorSumsToLimitOther limit = arrayOfSums
  where
    arrayOfSums :: UArray Int Int
    arrayOfSums = UA.array (1, limit) ((1,0):listProperFactors)
    listProperFactors = map sumProperFactors [2..limit]
    sumProperFactors :: Int -> (Int, Int)
    sumProperFactors value = (value, sumUniqueProperFactors)
      where
        sumUniqueProperFactors = sum uniqueProperFactors
        uniqueProperFactors = removeDupsAndValue factorsWithDupsSorted []
        factorsWithDupsSorted = sort (map product primeFactorSubsets)
        primeFactorSubsets = subsequences primeFactors
        primeFactors = map fromIntegral (myPrimeFactors (fromIntegral value))
        -- This function will remove duplicates from a sorted list, and will
        -- remove any elements that are equal to value.
        removeDupsAndValue :: [Int] -> [Int] -> [Int]
        removeDupsAndValue [] result = result
        removeDupsAndValue (x:xs) [] = if x == value
                                       then removeDupsAndValue xs []
                                       else removeDupsAndValue xs [x]
        removeDupsAndValue (x:xs) (y:ys) = if x == y || x == value
                                           then removeDupsAndValue xs (y:ys)
                                           else removeDupsAndValue xs (x:y:ys)

-- Generate all partitions of a list.  Note, there are 2^len of them, where len is the length of the
-- list.

allPartitions :: [a] -> [([a], [a])]
allPartitions inList = map (createPartition inList ([], [])) listOfPartitionBitVectors
  where
    listOfPartitionBitVectors = [0 .. 2 ^ length inList - 1]
    createPartition :: [a] -> ([a], [a]) -> Int64 -> ([a], [a])
    createPartition [] acc _ = acc
    createPartition (x : xs) (zeroElems, oneElems) bitVector
      | bitVector .&. 0x1 == 1
        = createPartition xs (zeroElems, x : oneElems) (shiftR bitVector 1)
      | otherwise
        = createPartition xs (x : zeroElems, oneElems) (shiftR bitVector 1)

-- Generate all partitions of a list where the first partition has a given number of elements.

partitionsOfLength :: [a] -> Int -> [([a],[a])]
partitionsOfLength xs len
  | len > lenXS = []
  | len == lenXS = [(xs,[])]
  | otherwise = pickPartition xs len lenXS ([],[])
  where
    !lenXS = length xs
    pickPartition :: [a] -> Int -> Int -> ([a], [a]) -> [([a], [a])]
    pickPartition [] _ _ _ = []
    pickPartition ys 0 _ (p1s, p2s) = [(p1s, ys ++ p2s)]
    pickPartition (y : ys) len' lenXS' (p1s, p2s)
      | lenXS' == len' = [((y : ys) ++ p1s, p2s)]
      | otherwise
        = pickPartition ys (len' - 1) (lenXS' - 1) (y : p1s, p2s)
          ++ pickPartition ys len' (lenXS' - 1) (p1s, y : p2s)

-- Generate all partitions into two parts of the given list.  This will generate them as needed.

all2Partitions :: [a] -> [([a], [a])]
all2Partitions [] = [([], [])]
all2Partitions (x : xs)
  = let deeperParts = all2Partitions xs
    in  foldr pairXWithEach [] deeperParts
  where
    pairXWithEach (dp1, dp2) acc = (x : dp1, dp2) : (dp1, x : dp2) : acc

-- Generate all possible partitions of the third argument list. Under typical circumstances, it
-- should be called like: allPartitionsInitial [] ([], []) listToPartition

allPartitionsInitial :: [([a], [a])] -> ([a], [a]) -> [a] -> [([a], [a])]
allPartitionsInitial acc curr [] = curr : acc
allPartitionsInitial acc (currFst, currSnd) (x : xs) = newAcc
  where
    fstAcc = allPartitionsInitial acc (x : currFst, currSnd) xs
    newAcc = allPartitionsInitial fstAcc (currFst, x : currSnd) xs

-- The following several functions deal with infinite continuous fractions.

-- Compute the continued fraction using the given number of elements from the list of continued
-- fraction values.  For example, approximating the square root of 23 to 20 steps:
-- continuedFractionNSteps (4, [1, 3, 1, 8]) = 211 % 44

continuedFractionNSteps :: (Int, [Int]) -> Int -> Rational
continuedFractionNSteps (initialSum, convergents) steps
  = (fromIntegral initialSum % 1) + sumContinuous (reverse $ take steps convergents)
    (0 % 1 :: Rational)
  where
    sumContinuous :: [Int] -> Rational -> Rational
    sumContinuous [] currSum = currSum
    sumContinuous (x : xs) currSum = sumContinuous xs newSum
      where
        addedNext = currSum + (fromIntegral x % 1)
        newSum = denominator addedNext % numerator addedNext

-- This function will generate a list of successively more accurate numbers rational numbers
-- as an approximation using continued fractions.  For example,
-- take 5 $ successiveTermsOfContinuedFraction (4, (cycle [1, 3, 1, 8]))
--          = [4 % 1,5 % 1,19 % 4,24 % 5,211 % 44]

successiveTermsOfContinuedFraction :: (Int, [Int]) -> [Rational]
successiveTermsOfContinuedFraction (base, []) = repeat (fromIntegral base % 1)
successiveTermsOfContinuedFraction (base, cfList) = stocf (cycle cfList) []
  where
    stocf :: [Int] -> [Int] -> [Rational]
    stocf [] _ = [0 % 1 :: Rational]
    stocf (x : xs) revNumbers = sumContinuous revNumbers (0 % 1 ::Rational)
                                : stocf xs (x : revNumbers)
    sumContinuous :: [Int] -> Rational -> Rational
    sumContinuous [] currSum = currSum + (fromIntegral base % 1)
    sumContinuous (x : xs) currSum = sumContinuous xs newSum
      where
        addedNext = currSum + (fromIntegral x % 1)
        newSum = denominator addedNext % numerator addedNext

-- Generates the continued fraction sequence for the continued fraction representing the square root
-- of the number passed in.  The first number in the pair returned will be the floor of the square
-- root, and the second of the pair is a list of the repeating continued fraction numbers out to
-- where the repeating begins.  If the square root is exactly the first number in the pair, then
-- this list will be empty.  For example: continuedSqrtFrac 23 = (4,[1,3,1,8])

continuedSqrtFrac :: Int -> (Int, [Int])
continuedSqrtFrac value = (base, repeatedSeq)
  where
    repeatedSeq = if null continuedFracSeqTriples
                  then []
                  else let repeats = fromJust (elemIndex (head continuedFracSeqTriples)
                                                 (tail continuedFracSeqTriples)) + 1
                       in map (\(x,_,_) -> x) $ take repeats continuedFracSeqTriples
    (base, continuedFracSeqTriples) = continuedSqrtFrac1 value

    -- Generate the base of the square root, and an infinite sequence of triples for the continued
    -- fraction.
    
    continuedSqrtFrac1 :: Int -> (Int, [(Int, Int, Int)])
    continuedSqrtFrac1 val = (base', result)
      where
        base' = fromJust (find (\x -> x * x > val) (if val <= 2500 then [1..] else [50..])) - 1
        result = if base' * base' == val then [] else continuedSqrtFrac' base' 1

        -- Takes the current top number and the current numerator, and recursively generates triplets
        -- infinitely for the continued fraction.

        continuedSqrtFrac' :: Int -> Int -> [(Int, Int, Int)]
        continuedSqrtFrac' currTop currNumer
          = (currCount, currTop, currNumer) : continuedSqrtFrac' currNext currDiv
          where
            !currDiv = (val - currTop * currTop) `quot` currNumer
            !currCount = (currTop + base') `quot` currDiv
            !currNext = -(currTop - currDiv * currCount)

-- Continued fraction representation of e, which is (2, [1,2,1,1,4,1,1,6,1,...])

continuousFractionRepresentationOfE :: (Int, [Int])
continuousFractionRepresentationOfE = (2, eList 2)
  where
    eList val = 1 : val : 1 : eList (val + 2)

-- Returns successive integral solutions to Pell's Equation, which is x^2 - n*y^2 = 1.  Solutions
-- are found by using successive approximations of the continuous fraction representation of the
-- square root of n, and looking for one that solves this equation.  Once found, further solutions
-- are found using a formula based on the first solution and the most recent.  The pair (1, 0) is
-- always the first solution.

pellsEquationSolutions :: Int -> [(Integer, Integer)]
pellsEquationSolutions n
  | nSqrtSq == n = [(1, 0)]
  | otherwise = (1, 0) : firstSol : nextPellSolutions firstSol
  where
    nInteger = fromIntegral n
    nSqrt = fromIntegral (isqrt (fromIntegral n))
    nSqrtSq = nSqrt * nSqrt
    sqrtContinuous = continuedSqrtFrac n
    sqrtApproximations = successiveTermsOfContinuedFraction sqrtContinuous
    firstPellsSolutionRat = fromJust
                            $ find (\approx -> let numer = numerator approx :: Integer
                                                   denom = denominator approx :: Integer
                                               in (numer * numer) - nInteger * (denom * denom) == 1)
                                   sqrtApproximations
    firstSolX = numerator firstPellsSolutionRat
    firstSolY = denominator firstPellsSolutionRat
    firstSol = (firstSolX, firstSolY)
    nextPellSolutions (lastX, lastY) = nextSol : nextPellSolutions nextSol
      where
        nextSol = (firstSolX * lastX + nInteger * firstSolY * lastY,
                   firstSolX * lastY + firstSolY * lastX)

-- Returns successive integral solutions to the negative Pell's Equation, which is x^2 - n*y^2 = -1.
-- Solutions are found by using successive approximations of the continuous fraction representation
-- of the square root of n, and looking for ones that solve this equation. Not all will have a
-- solutions, and the only ones that do are the ones where the length of the repeated continuous
-- fraction sequence for square root of the number is odd.  Also, 1 has a single solution.  Once the
-- first solution is found, there doesn't seem to be a simple iteration to get the next ones, so
-- just continue to search the continuous fraction sequence.

pellsEquationNegOneSolutions :: Int -> [(Integer, Integer)]
pellsEquationNegOneSolutions n
  | n == 1 = [(0,1)]
  | even (length cf) = []
  | otherwise = solutions
  where
    sqrtContinuous@(_, cf) = continuedSqrtFrac n
    nInteger = fromIntegral n :: Integer
    sqrtApproximations = successiveTermsOfContinuedFraction sqrtContinuous
    solutions = foldr (\approx acc -> let numer = numerator approx :: Integer
                                          denom = denominator approx :: Integer
                                      in if (numer * numer) - nInteger * (denom * denom) == -1
                                         then (numer, denom) : acc else acc)
                      [] sqrtApproximations

-- Prints all possible expression results for the four arithmetical operations given a list of up to
-- 4 numbers.

printAllArithPerms :: [Int] -> IO ()
printAllArithPerms intList = do
  let allPerms = allArithPerms intList
  printEach allPerms
  return ()
  where
    printEach [] = return ()
    printEach ((val, strs) : xs) = do
      putStrLn $ show val ++ ": " ++ show strs
      printEach xs
      return ()

-- Return a list of all arithmetical results combining the numbers in the input list.  For example:
-- allArithPerms [2,3] = (-1,["2-3"]),(1,["3-2"]),(5,["2+3"]),(6,["2*3"])]
-- It will handle lists of up to four numbers.  Each number in the result will be associated with
-- all expressions that generate it. Parenthesis will be added as needed.
-- A faster solution to just get the answer would not generate the associated expression strings,
-- but these might be interesting to have, and they aren't actually computed unless they are used.

allArithPerms :: [Int] -> [(Int, [String])]
allArithPerms intList = (collectLike (0, []) . sortBy (\(f, _) (s, _) -> compare f s)
                          . map (BF.first (fromIntegral . numerator))
                          . filter (\(x, _) -> denominator x == 1) . allArithPerms')
                        (map (\i -> (fromIntegral i % 1, show i)) intList)
  where

    -- Walk through the sorted list and collect all of the expression strings resulting in the same
    -- answer into a single list associated with that answer.

    collectLike :: (Int, [String]) -> [(Int, String)] -> [(Int, [String])]
    collectLike (val, strs) []
      | null strs = []
      | otherwise = [(val, strs)]
    collectLike (val, strs) ((newVal, newStr) : xs)
      | null strs = collectLike (newVal, [newStr]) xs
      | val == newVal = collectLike (val, newStr : strs) xs
      | otherwise = (val, strs) : collectLike (newVal, [newStr]) xs

    -- Find all results and associated expression strings associated with the list of inputs. This
    -- uses rational numbers so that fractions that are later canceled by multiplication aren't
    -- lost.

    allArithPerms' :: [(Rational, String)] -> [(Rational, String)]
    allArithPerms' intPairList
      | null intList = []
      | listLen == 1 = intPairList
      | listLen == 2 = arithPermsOf2 first second
      | listLen == 3
        = concat [concatMap (arithPermsOf2 first) permsSecondThird,
                  concatMap (arithPermsOf2 second) permsFirstThird,
                  concatMap (arithPermsOf2 third) permsFirstSecond]
      | listLen == 4
        = concat [concatMap (arithPermsOf2 first) (allArithPerms' [second, third, fourth]),
                  concatMap (arithPermsOf2 second) (allArithPerms' [first, third, fourth]),
                  concatMap (arithPermsOf2 third) (allArithPerms' [first, second, fourth]),
                  concatMap (arithPermsOf2 fourth) (allArithPerms' [first, second, third]),
                  concat [arithPermsOf2 x y | x <- permsFirstSecond, y <- permsThirdFourth],
                  concat [arithPermsOf2 x y | x <- permsFirstThird, y <- permsSecondFourth],
                  concat [arithPermsOf2 x y | x <- permsFirstFourth, y <- permsSecondThird]]
      | otherwise = []
      where
        listLen = length intPairList
        first  = head intPairList
        second = intPairList !! 1
        third  = intPairList !! 2
        fourth = intPairList !! 3
        permsFirstSecond  = arithPermsOf2 first second
        permsFirstThird   = arithPermsOf2 first third
        permsFirstFourth  = arithPermsOf2 first fourth
        permsSecondThird  = arithPermsOf2 second third
        permsSecondFourth = arithPermsOf2 second fourth
        permsThirdFourth  = arithPermsOf2 third fourth

    -- Take in two numbers, along with a string representation of each, and combine using the four
    -- arithmetic operations. Make sure we don't divide by zero.  Return the list of all resulting
    -- values of the operations, and the strings with thise added expressions.

    arithPermsOf2 :: (Rational, String) -> (Rational, String) -> [(Rational, String)]
    arithPermsOf2 (firstVal, firstStr) (secondVal, secondStr) = reverse withDivByFirst
      where
        withDivByFirst
          = if firstVal == 0
            then withDivBySecond
            else  (secondVal / firstVal, combineArith secondStr "/" firstStr) : withDivBySecond
        withDivBySecond
          = if secondVal == 0
            then nonDivResults
            else (firstVal / secondVal, combineArith firstStr "/" secondStr) : nonDivResults
        nonDivResults = [(firstVal * secondVal, combineArith firstStr "*" secondStr),
                         (secondVal - firstVal, combineArith secondStr "-" firstStr),
                         (firstVal - secondVal, combineArith firstStr "-" secondStr),
                         (firstVal + secondVal, combineArith firstStr "+" secondStr)]
        combineArith first op second = addParensIfNeeded first ++ op ++ addParensIfNeeded second
        addParensIfNeeded str
          = if isNothing (find (not . isDigit) str) then str else '(' : (str ++ ")")

-- Remove duplicates from a sorted list.  An empty list must be passed in as the first argument, and
-- the resulting list will be in reverse order.
-- This is about 2x slower than the three variants below if you don't care about reversing the list,
-- but if you need to reverse the list after running this, then this is about 3x slower.

removeDups :: (Eq a) => [a] -> [a] -> [a]
removeDups result [] = result
removeDups [] (x:xs) = removeDups [x] xs
removeDups yss@(y:_) (x:xs) = if x == y
                           then removeDups yss xs
                           else removeDups (x:yss) xs

-- This lazy version removes the duplicates in the list leaving them in the same order.
-- This is ever so slightly slower (1%) than the two using foldr below.

removeDupsInOrder :: (Eq a) => [a] -> [a]
removeDupsInOrder [] = []
removeDupsInOrder (x : xs) = x : compress x xs
  where
    compress _ [] = []
    compress z (y : ys)
      | y == z = compressRest
      | otherwise = y : compressRest
      where compressRest = compress y ys

-- This version is lazy, leaves the list in the same order, and uses the three-argument foldr
-- function technique. This is als a fast as it gets, equivalent to removeDupsInOrderFoldr2. It is a
-- good example of this foldr technique.
-- The second argument to foldr is a function that takes an 'a' value and returns an '[a]'. In the
-- base case of the foldr function when the input list is empty the function (const []) is used so
-- that no matter the second parameter, [] is returned. This is more verbose than it has to be, but
-- it helps with understanding.
--
-- Here is the definition of foldr:
--  foldr :: (a -> b -> b) -> b -> [a] -> b
--  foldr f e [] = e
--  foldr f e (x:xs) = f x (foldr f e xs)
--
-- Here is a trace of the foldr part given the input [1,1,2,3,3]
--
-- 1 : (foldr nextElemFn (const []) [1,2,3,3]) 1
-- 1 : nextElemFn 1 (foldr nextElemFn (const []) [2,3,3]) 1
-- 1 : (restOfListFn 1) -- The currVal 1 and the accumulator function are implicit in the scope.
-- 1 : nextElemFn 2 (foldr nextElemFn (const []) [3,3]) 1
-- 1 : (restOfListFn 1) -- The currVal 2 and the accumulator function are implicit in the scope.
-- 1 : 2 : nextElemFn 3 (foldr nextElemFn (const []) [3]) 2
-- 1 : 2 : (restOfListFn 2) -- The currVal 3 and the accumulator function are implicit in the scope.
-- 1 : 2 : 3 : nextElemFn 3 (foldr nextElemFn (const []) []) 3
-- 1 : 2 : 3 : (restOfListFn 3)
-- 1 : 2 : 3 : (const []) 3
-- 1 : 2 : 3 : []

removeDupsInOrderFoldr :: forall a. (Eq a) => [a] -> [a]
removeDupsInOrderFoldr [] = []
removeDupsInOrderFoldr (x : xs) = x : foldr nextElemFn (const []) xs x
  where

    -- Take in the current element and the accumulator function (a -> [a]), which comes from below
    -- in the recursion, and return a function (a -> [a]) that takes in the last parameter (3rd to
    -- the foldr function), and because of its context, is able to compare it to the current value
    -- to determine whether the current value is different from the last or not.

    nextElemFn :: a -> (a -> [a]) -> a -> [a]
    nextElemFn currVal fn = restOfListFn
      where

        -- This function is returned and takes the third argument and return a list based on it and
        -- the deeper search of foldr when needed by executing the function that is the accumulator.
        -- This works because this function has access to currVal and fn from nextElemFn. Notice
        -- that the next iteration of foldr isn't triggered until fn is called, so if currVal is
        -- different from lastVal, then it is returned in a lazy manner.

        restOfListFn :: a -> [a]
        restOfListFn lastVal
          | currVal == lastVal = fn currVal
          | otherwise = currVal : fn currVal

-- This is the same idea as the one above, but doesn't declare the restOfListFn function. I don't
-- know which is easier to understand. They run at the same speed.

removeDupsInOrderFoldr2 :: forall a. (Eq a) => [a] -> [a]
removeDupsInOrderFoldr2 [] = []
removeDupsInOrderFoldr2 (x : xs) = x : foldr nextElemFn (const []) xs x
  where

    -- Take in the current element and the accumulator function (a -> [a]), which comes from below
    -- in the recursion, and the last value seen, which is the third parameter to foldr. If the
    -- currVal and lastVal are different, then return the currVal first, then call the function
    -- which is the accumulator passed to foldr, giving it the currVal as the third parameter.

    nextElemFn :: a -> (a -> [a]) -> a -> [a]
    nextElemFn currVal fn lastVal
      | currVal == lastVal = fn currVal
      | otherwise = currVal : fn currVal

-- This technique uses standard operations, but is over 3x slower than the three just above.

removeDupsGroup :: (Eq a) => [a] -> [a]
removeDupsGroup = map head . group

-- For the input list (usually sorted) return a list of pairs with each unique element and a count
-- of sequential duplicates. This is done with traditional tail recursion and performs well. For
-- example: countDups [1,1,2,2,3,3,3] = [(1,2),(2,2),(3,3)]
-- Note that no sorting is done in this routine, so identical elements that are not sequential are
-- counted separately.

countDups :: (Eq a) => [a] -> [(a, Int)]
countDups [] = []
countDups (x : xs) = cd (x, 1) xs
  where
    cd currValAndCount [] = [currValAndCount]
    cd currValAndCount@(currVal, currCount) (y : ys)
      | currVal == y = let newCount = currCount + 1
                       in newCount `seq` cd (currVal, newCount) ys
      | otherwise = currValAndCount : cd (y, 1) ys

-- Same as countDups, but uses foldr with a fourth parameter to keep track of the current value.

countDupsFoldr :: forall a. (Eq a) => [a] -> [(a, Int)]
countDupsFoldr [] = []
countDupsFoldr (x : xs) = foldr handleElemFn lastFn xs (x, 1)
  where
    lastFn :: (a, Int) -> [(a, Int)]
    lastFn remainingValAndCount = [remainingValAndCount]

    handleElemFn :: a -> ((a, Int) -> [(a, Int)]) -> (a, Int) -> [(a, Int)]
    handleElemFn currVal fn savedValAndCount@(savedVal, savedCount)
      | currVal == savedVal = let newSavedCount = savedCount + 1
                              in newSavedCount `seq` fn (savedVal, newSavedCount)
      | otherwise = savedValAndCount : fn (currVal, 1)

-- This function assumes that both lists are sorted, and returns the list where all of the elements
-- of the first list are removed from the second. Either or both lists may be infinite.

removeElementsSorted :: (Eq a, Ord a) => [a] -> [a] -> [a]
removeElementsSorted [] xs = xs
removeElementsSorted _ [] = []
removeElementsSorted allR@(r : rs) allX@(x : xs)
  | r == x = removeElementsSorted rs xs
  | r < x  = removeElementsSorted rs allX
  | otherwise = x : removeElementsSorted allR xs

-- Duplicate functionality to removeElementsSorted.
---- Return the list from removing the elements of the second from the first. The lists are
---- assumed to be in sorted order.
--
--removeSecondList :: (Ord a, Eq a) => [a] -> [a] -> [a]
--removeSecondList [] _ = []
--removeSecondList firstL [] = firstL
--removeSecondList xss@(x : xs) yss@(y : ys)
--  | x < y = x : removeSecondList xs yss
--  | x > y = removeSecondList xss ys
--  | otherwise = removeSecondList xs yss

-- Return the union of the two sorted lists.

intersectionSorted :: (Eq a, Ord a) => [a] -> [a] -> [a]
intersectionSorted [] _ = []
intersectionSorted _ [] = []
intersectionSorted xss@(x : xs) yss@(y : ys)
  | x < y = intersectionSorted xs yss
  | x > y = intersectionSorted xss ys
  | otherwise = x : intersectionSorted xs ys

-- Compute init and last in the same call.

initLast :: [a] -> ([a], a)
initLast [] = error "Function initLast called with empty list."
initLast [x] = ([], x)
initLast (x : xs) = let (xs', y) = initLast xs
                    in (x : xs', y)

-- Generate all subsets of a list.

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- Generate all subsets of length n of a given list.

--subsetsOfSize :: Int -> [a] -> [[a]]
--subsetsOfSize 0 _ = [[]]
--subsetsOfSize _ [] = []
--subsetsOfSize n (x:xs) = (subsetsOfSize n xs) ++ map(x:) (subsetsOfSize (n-1) xs)

subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize n xs
  | n < 1 = []
  | otherwise = subsetsOfSize' n (length xs) xs
  where
    subsetsOfSize' :: Int -> Int -> [b] -> [[b]]
    subsetsOfSize' 0 _ _ = [[]]
    subsetsOfSize' _ _ [] = []
    subsetsOfSize' n' len (y : ys)
      | n' > len = []
      | n' == len = [y : ys]
      | otherwise = subsetsOfSize' n' (len-1) ys ++ map (y :) (subsetsOfSize' (n'-1) (len-1) ys)

-- Generate all possible pairs of disjoint subsets of size n.  The first list will always contain
-- the earliest element of the origianal list.

disjointSubsetPairsOfSize :: [a] -> Int -> [([a], [a])]
disjointSubsetPairsOfSize lis n
  | n > listLength `quot` 2 || n < 1 = []
  | otherwise = dspos 0 listLength [] [] lis
  where
    listLength = length lis
    dspos currSize restCount firstSubset notUsed rest

      -- We have filled up the first list with the needed number of elements, pair it with all of
      -- the subsets of those remaining as well as those not used after the initial element. The
      -- first list will always have the minimum lowest element.

      | currSize == n = zip (repeat (reverse firstSubset))
                            (subsetsOfSize n (reverse notUsed ++ rest))

      -- If there aren't enough members of the rest of the list to fill the first subset list, then
      -- return the null list.

      | restCount < (n - currSize) = []

      -- Concatenate the results from adding the current element to the first subset list with the
      -- results of not adding it.

      | otherwise
        = let newRestCount = restCount - 1
              newNotUsed = if currSize == 0 then notUsed else head rest : notUsed
          in  dspos (currSize + 1) newRestCount (head rest : firstSubset) notUsed (tail rest)
                ++ dspos currSize newRestCount firstSubset newNotUsed (tail rest)

-- Generate all subset products of a list.

subsetsProd :: (Integral a) => [a] -> [a]
subsetsProd [] = [1]
subsetsProd (x:xs) = let nonXSubsetsProd = subsetsProd xs
                     in nonXSubsetsProd ++ map (x*) nonXSubsetsProd

-- Generate all possible products out of a list of (count, value) pairs. Usually these would be
-- (count, primeFactor) paris. For example:
-- subsetsProdPF [(2,2), (1,3), (2,5)] = [1,5,25,3,15,75,2,10,50,6,30,150,4,20,100,12,60,300]
-- Note that the products won't be in order, but 1 will always be first and the largest will
-- always be last.

subsetsProdPF :: (Integral a) => [(Int, a)] -> [a]
subsetsProdPF [] = [1]
subsetsProdPF ((count, pf) : rest) = result
  where
    withoutThisOneSubsetsProd = subsetsProdPF rest
    result = withoutThisOneSubsetsProd ++ foldr multByPFPower [] [1..count]
    multByPFPower pow acc = map (\x -> x * pf ^ pow) withoutThisOneSubsetsProd ++ acc

-- Generate all of the divisors of n including 1 and n. The divisors won't necessarily be in order,
-- but 1 will be the first on the list and n will be last.

divisors :: Int64 -> [Int64]
divisors n = listOfDivisors
  where
    nPF = (groupPrimeFactorsIntoPairs . map fromIntegral . myPrimeFactors) n
    listOfDivisors = subsetsProdPF nPF

-- Generate all pairs of the list with the first element always being before the second in the list.

pairsOrdered :: [a] -> [(a, a)]
pairsOrdered [] = []
pairsOrdered [_] = []
pairsOrdered (x : xs) = map (x,) xs ++ pairsOrdered xs

-- Count the unique values in a sorted list.

countUniqueInSortedList :: (Eq a) => [a] -> Int
countUniqueInSortedList [] = 0
countUniqueInSortedList (x : xs) = cUISL 1 x xs
  where
    cUISL :: (Eq b) => Int -> b -> [b] -> Int
    cUISL count _ [] = count
    cUISL count lastVal (y : ys) = if y == lastVal
                                   then cUISL count lastVal ys
                                   else cUISL (count + 1) y ys

-- Generate permutations in lexigraphic order, assuming the input in sorted.
-- permsLexicographic [1,2,3] = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

permsLexicographic :: [a] -> [[a]]
permsLexicographic [] = [[]]
permsLexicographic (x : xs) = concatMap (\(p, ps) -> map (p :) (permsLexicographic ps))
                                        (pairsEachAndRestSameOrder x [] xs)
  where
    pairsEachAndRestSameOrder y behind [] = [(y, reverse behind)]
    pairsEachAndRestSameOrder y behind inFront
      = (y, foldl' (flip (:)) inFront behind)
        : pairsEachAndRestSameOrder (head inFront) (y : behind) (tail inFront)

-- Generate all pairs of an element of digitsLeft and the list of all of the
-- remaining elements.  This algorithm is single-pass and O(n).
-- The order of the remaining list is not sorted.
-- Example: allPairsFirstAndRest [1,2,3,4]
--                        = [(4,[3,2,1]),(3,[2,1,4]),(2,[1,3,4]),(1,[2,3,4])]

allPairsFirstAndRest :: [a] -> [(a, [a])]
allPairsFirstAndRest fullList = aPFARIter fullList [] []
  where
    aPFARIter :: [a] -> [a] -> [(a, [a])] -> [(a, [a])]
    aPFARIter [] _ resultSoFar = resultSoFar
    aPFARIter (d : ds) digitsAlreadyUsed resultSoFar
      = aPFARIter ds (d : digitsAlreadyUsed)
                  ((d, digitsAlreadyUsed ++ ds) : resultSoFar) 

-- Generate a list of lists where each list has count elements,
-- the members sum to totalSum, and whose elements are in the
-- range from minVal to maxVal.  For example:
--   allWays 2 (1,10) 17 == [[7,10],[8,9],[9,8],[10,7]]
--   allWays 2 (1,10) 40 == []
--   allWays 3 (1,10) 28 ==
--           [[8,10,10],[9,9,10],[10,8,10],[9,10,9],[10,9,9],[10,10,8]]

allWays :: Int -> (Int, Int) -> Int -> [[Int]]
allWays count (minVal, maxVal) totalSum
  = allWays' count calcMin (calcMin, calcMax) totalSum [] []
  where
    (calcMin, calcMax) = newMinMax count (minVal, maxVal) totalSum

    -- Iteratively do a depth first search for all lists whose
    -- elements are in the requested range and where the sum of the
    -- elements add up to the total requested.
                         
    allWays' :: Int -> Int -> (Int, Int) -> Int -> [Int] -> [[Int]] -> [[Int]]
    allWays' 0 _ _ _ currFull answersSoFar = currFull : answersSoFar
    allWays' elementsRemaining currVal (minVal', maxVal') totalRemainingSum
             currPartial answersSoFar
      | currVal > maxVal' = answersSoFar
      | otherwise = allAnswersFromHere
      where
        answersWithCurr = allWays' newElementsRemaining newMin (newMin, newMax)
                                   newRemainingSum newPartial answersSoFar
        allAnswersFromHere = allWays' elementsRemaining (currVal + 1)
                                      (minVal', maxVal') totalRemainingSum
                                      currPartial answersWithCurr
        (newMin, newMax) = newMinMax newElementsRemaining (minVal', maxVal')
                                     newRemainingSum
        newElementsRemaining = elementsRemaining - 1
        newRemainingSum = totalRemainingSum - currVal
        newPartial = currVal : currPartial

    -- Given the number of elements we have to work with and the
    -- current min and max values, calculate the min and max values
    -- for the current element.  If there is no valid value, then the
    -- min returned will be greater than the max returned.
    -- For example:
    --   newMinMax 1 (1,10)  5 == (5,5)
    --   newMinMax 1 (1,10) 12 == (12,10)
    --   newMinMax 2 (1,10) 20 == (10,10)
    --   newMinMax 2 (1,10) 18 == (8,10)
                         
    newMinMax :: Int -> (Int, Int) -> Int -> (Int, Int)
    newMinMax cnt (currMin, currMax) totalSum'
      = let !countMinusOne = cnt - 1
        in (max currMin (totalSum' - (currMax * countMinusOne)),
            min currMax (totalSum' - (currMin * countMinusOne)))

-- Generate a list of lists of integral values where the values from low to high appear spread out
-- into the given number of groups.
-- For example:
--   spreadList 1 20 8 = [[1,9,17],[2,10,18],[3,11,19],[4,12,20],[5,13],[6,14],[7,15],[8,16]]

spreadList :: (Integral a) => a -> a -> a -> [[a]]
spreadList low high groups
  = filter (not . null) $ map (\x -> [x, (x + groups)..high]) [low..(low + groups - 1)]

spreadListSkip :: (Integral a) => a -> a -> a -> a -> [[a]]
spreadListSkip low high skip groups
  = filter (not . null) $ map (\x -> [x, (x + (groups * skip))..high])
                                [low,(low + skip)..(low + (groups * skip) - 1)]

-- This function is similar to spreadList, but it works on an actual list of items, not just a range
-- of numbers. This is very helpful to parallelize mapping to a list of starts where the time taken
-- isn't uniform or random, but bunches up at a particular part of the list, which can cause a
-- parListChunk strategy to put most of the time-consuming ones in the same chunk.
-- For example (spreadOutList 4 primes) = [[2, 11, ...], [3, 13, ...], [5, 17, ...], [7, 19, ...]].

spreadOutList :: Int -> [a] -> [[a]]
spreadOutList listCount [] = replicate listCount []
spreadOutList listCount zs = zipConsFull firstN (spreadOutList listCount rest)
  where
    (firstN, rest) = splitAt listCount zs
    zipConsFull _ [] = []
    zipConsFull [] (y : ys) = y : zipConsFull [] ys
    zipConsFull (x : xs) (y : ys) = (x : y) : zipConsFull xs ys

-- Split the given list based on a split function that returns a boolean for elements of the list.
-- Those returning True will go to the first output list in the pair and those returning False
-- will go to the second output list in the pair.  This will work for infinite lists input.
-- Note that we use lazy matching (~) for the accumulator, otherwise we couldn't use this with
-- infinite lists because it would have to match the structure of the pair, so it would have to
-- go all the way to the end.

splitListFn :: (a -> Bool) -> [a] -> ([a], [a])
splitListFn splitFn = foldr accFn ([], [])
  where
    accFn element ~(xs, ys)
      | splitFn element = let newXs = element : xs in newXs `seq` (newXs, ys)
      | otherwise = let newYs = element : ys in newYs `seq` (xs, newYs)

splitList :: [a] -> ([a], [a])
splitList = foldr (\a ~(xs, ys) -> (a : ys, xs)) ([], [])

-- Break the given range into chunkCnt ranges in a list. The last range will usually be a bit longer
-- than the others to make up for the difference if the size of the initial range isn't evenly
-- divisible by the number of chunks requested. For example:
--   breakRangeIntoChunks 5 (1,50) = [(1,10),(11,20),(21,30),(31,40),(41,50)]

breakRangeIntoChunks :: Int -> (Integer, Integer) -> [(Integer, Integer)]
breakRangeIntoChunks chunkCnt (low, high)
  | high < low = error "Range must not be empty."
  | (high - low + 1) < fromIntegral chunkCnt = error "Range must be larger than number of chunks."
  | otherwise = genRanges chunkCnt low
  where
    chunkSize = (high - low + 1) `div` fromIntegral chunkCnt
    genRanges 1 start = [(start, high)]
    genRanges n start
      | n < 1 = error "Chunk count must be 1 or greater."
      | otherwise = let nextStart = start + chunkSize
                    in nextStart `seq` (start, nextStart - 1) : genRanges (n - 1) nextStart

-- Check the entries of the given list for them all being equal. Return the value if they are all
-- equal and Nothing if not, as the first entry in the pair. The second entry is a Boolean
-- indicating whether all entries are equal, which is also true for an empty list.

allEqual :: (Eq a) => [a] -> (Maybe a, Bool)
allEqual [] = (Nothing, True)
allEqual (x : xs) = ae (Just x, True) xs
  where
    ae acc [] = acc
    ae acc@(Just x', _) (y : ys)
      | y == x' = ae acc ys
      | otherwise = (Nothing, False)
    ae (Nothing, _) _ = error "Undefined case in allEqual."

-- Take in a string of digits and return an Int that it represents.

stringToInt :: String -> Int
stringToInt xs = stringToInt' xs 0
  where
    ordZero = ord '0'
    stringToInt' :: String -> Int -> Int
    stringToInt' [] val = val
    stringToInt' (y:ys) val = stringToInt' ys (val * 10 + (ord y - ordZero))

-- Convert an int to a list of ints.

intToListOfInts :: Int -> [Int]
intToListOfInts value = intToListOfInts' value []
  where
    intToListOfInts' 0 xs = xs
    intToListOfInts' val xs
      = let (quotient, remainder) = val `quotRem` 10
        in intToListOfInts' quotient (remainder : xs)

int64ToListOfInts :: Int64 -> [Int]
int64ToListOfInts value = int64ToListOfInts' value []
  where
    int64ToListOfInts' 0 xs = xs
    int64ToListOfInts' val xs
      = let (quotient, remainder) = val `quotRem` 10
        in int64ToListOfInts' quotient (fromIntegral remainder : xs)

integerToListOfInts :: Integer -> [Int]
integerToListOfInts value = integerToListOfInts' value []
  where
    integerToListOfInts' 0 xs = xs
    integerToListOfInts' val xs
      = let (quotient, remainder) = val `quotRem` 10
        in integerToListOfInts' quotient (fromIntegral remainder : xs)

intToListOfIntsBase :: Int -> Int -> [Int]
intToListOfIntsBase base value = intToListOfInts' value []
  where
    intToListOfInts' 0 xs = xs
    intToListOfInts' val xs
      = let (quotient, remainder) = val `quotRem` base
        in remainder `seq` intToListOfInts' quotient (remainder : xs)

integerToListOfIntsBase :: Integer -> Integer -> [Int]
integerToListOfIntsBase base value = integerToListOfInts' value []
  where
    integerToListOfInts' 0 xs = xs
    integerToListOfInts' val xs
      = let (quotient, remainder) = val `quotRem` base
        in integerToListOfInts' quotient (fromIntegral remainder : xs)

-- Convert a list of ints to a single int.  The inverse of intToListOfInts.

intListToInt :: [Int] -> Int
intListToInt = foldl' (\acc x -> acc * 10 + x) 0

intListToInt64 :: [Int] -> Int64
intListToInt64 = foldl' (\acc x -> acc * 10 + fromIntegral x) 0

intListToInteger :: [Int] -> Integer
intListToInteger = foldl' (\acc x -> acc * 10 + fromIntegral x) 0

intListToIntegerBase :: Integer -> [Int] -> Integer
intListToIntegerBase base = foldl' (\acc x -> acc * base + fromIntegral x) 0

-- Returns true if the digits in the two numbers are permutations of each other.  Note that the
-- simple way to check this is to convert to strings, sort, and then see if the lists are equal.
-- This is slow, so first we can quickly check that they are equal mod 9, which they have to be if
-- they are a permuted pair.  The variations that follow of this program either don't do the mod
-- check (if it was already done by the caller), and use accumulation vectors for the digits.  The
-- one that does the sort seems to be the fastest surprisingly.

permutedPair :: (Integral a, Show a) => a -> a -> Bool
permutedPair value1 value2
  = (value1 `rem` 9) == (value2 `rem` 9) && sort (show value1) == sort (show value2)

permutedPairNoMod :: (Integral a, Show a) => a -> a -> Bool
permutedPairNoMod value1 value2 = sort (show value1) == sort (show value2)

permutedPair1 :: (Integral a, Eq a) => a -> a -> Bool
permutedPair1 value1 value2
  = (value1 `rem` 9) == (value2 `rem` 9) && (digitFreq1 == digitFreq2)
  where
    digitFreq1 :: UV.Vector Int
    digitFreq1 = UV.accum (+) (UV.replicate 10 0) (digitIncList value1)
    digitFreq2 :: UV.Vector Int
    digitFreq2 = UV.accum (+) (UV.replicate 10 0) (digitIncList value2)
    digitIncList :: (Integral b) => b -> [(Int, Int)]
    digitIncList 0 = []
    digitIncList value = let !lowDigit = fromIntegral (value `rem` 10)
                             !nextFactor10 = value `quot` 10
                         in (lowDigit, 1) : digitIncList nextFactor10

permutedPairNoMod1 :: (Integral a, Eq a) => a -> a -> Bool
permutedPairNoMod1 value1 value2 = digitFreq1 == digitFreq2
  where
    digitFreq1 :: UV.Vector Int
    digitFreq1 = UV.accum (+) (UV.replicate 10 0) (digitIncList value1)
    digitFreq2 :: UV.Vector Int
    digitFreq2 = UV.accum (+) (UV.replicate 10 0) (digitIncList value2)
    digitIncList :: (Integral b) => b -> [(Int, Int)]
    digitIncList 0 = []
    digitIncList value = let !lowDigit = fromIntegral (value `rem` 10)
                             !nextFactor10 = value `quot` 10
                         in (lowDigit, 1) : digitIncList nextFactor10

-- Generate all unique permutations of a list of items with associated counts.
-- For example: permsWithDups [(2,8),(3,9)] is:
--   [[8,8,9,9,9],[8,9,9,9,8],[8,9,9,8,9],[8,9,8,9,9],[9,9,9,8,8],[9,9,8,8,9],
--    [9,9,8,9,8],[9,8,8,9,9],[9,8,9,9,8],[9,8,9,8,9]]

permsWithDups :: (Eq a) => [(Int, a)] -> [[a]]
permsWithDups [] = []
permsWithDups [(c, x)] = [replicate c x]
permsWithDups xs = firstOfEach [] xs
  where
    firstOfEach _ [] = []
    firstOfEach ys ((c, z) : zs) = map (z :) permsOfOthers ++ firstOfEach ((c, z) : ys) zs
      where
        permsOfOthers
          | c == 1 = permsWithDups otherPairs
          | otherwise = let newC = c - 1
                        in  newC `seq` permsWithDups ((newC, z) : otherPairs)
        otherPairs = ys ++ zs

--byteStringToInt :: BC.ByteString -> Int
--byteStringToInt xs = stringToInt' xs 0
--  where
--    ordZero = ord '0'
--    stringToInt' :: BC.ByteString -> Int -> Int
--    stringToInt' "" val = val
--    stringToInt' (x:xs) val = stringToInt' xs (val*10 + ((ord x) - ordZero))

-- Return true if the given integral number is a palindrome.

palindromic :: (Integral a, Show a) => a -> Bool
palindromic number = numberStr == reverse numberStr
  where numberStr = show number

-- Returns true if the passed in number is pandigital (has all digits from 1 to
-- n where the number is n digits long.

pandigitValuesForNumberLengthN :: UArray Int Int
pandigitValuesForNumberLengthN
  = UA.array (0, 9) [(0,0), (1,0x2), (2,0x6), (3,0xe), (4,0x1e), (5,0x3e),
                     (6,0x7e), (7,0xfe), (8,0x1fe), (9,0x3fe)]

-- Is the number pandigital in base 10 not including 0.

isPandigital :: Int -> Bool
isPandigital value
  | lenValue > 9 = False
  | otherwise = bitsOfDigits == (pandigitValuesForNumberLengthN UA.! lenValue)
  where
    lenValue = length strValue
    strValue = show value
    ord0 = ord '0'
    bitsOfDigits = foldl' setBitsForDigits 0x0 strValue
    setBitsForDigits :: Int -> Char -> Int
    setBitsForDigits acc x = acc .|. shiftL 0x1 (ord x - ord0)

-- Return true if the given number contains all of the 'digits' in this base.  Use bits to keep
-- track of 'digits' encountered so far, then compare agains an int with all appropriate bits set.

isPandigitalBase :: Int64 -> Int -> Bool
isPandigitalBase value base = setDigitBits value 0x0 == ((2^base - 1) :: Int64)
  where
    setDigitBits :: Int64 -> Int64 -> Int64
    setDigitBits 0 bits = bits
    setDigitBits val bits = newBits `seq` setDigitBits q newBits
      where (q,r) = val `quotRem` fromIntegral base
            newBits = bits .|. shiftL 0x1 (fromIntegral r)

-- Return true if the given number is pandigital is all bases from 2 to maxBase.

isSuperPandigital :: Int -> Int64 -> Bool
isSuperPandigital maxBase value = all (isPandigitalBase value)
                                  [maxBase, (maxBase-1)..2]

type AccWords = ([BC.ByteString], String, Bool)
parseWordsFromString :: AccWords -> Char -> AccWords
parseWordsFromString (namesSoFar, currName, True) currChar
  | isAlpha currChar = (namesSoFar, currChar : currName, True)
  | otherwise = (BC.pack (reverse currName) : namesSoFar, [], False)
parseWordsFromString (namesSoFar, _, False) currChar
  | isAlpha currChar = (namesSoFar, [currChar], True)
  | otherwise = (namesSoFar, [], False)

-- Parse the ints from a string where the ints are separated by non-digits.  Return the ints as a list.

parseIntsFromString :: String -> [Int]
parseIntsFromString input = parseIntsFromString' input 0 False
  where
    parseIntsFromString' :: String -> Int -> Bool -> [Int]
    parseIntsFromString' [] acc inDigits
      | inDigits = [acc]
      | otherwise = []
    parseIntsFromString' (x : xs) acc inDigits
      | isDigit x = parseIntsFromString' xs (acc * 10 + digitToInt x) True
      | inDigits = acc : parseIntsFromString' xs 0 False
      | otherwise = parseIntsFromString' xs 0 False

-- Compute the product of [n,n-1,n-2,..,m].

prodNgtM :: Int -> Int -> Integer
prodNgtM n m = prodNgtM' n 1
  where
    prodNgtM' :: Int -> Integer -> Integer 
    prodNgtM' n' currResult
      | n' > m = prodNgtM' (n' - 1) (currResult * fromIntegral n')
      | otherwise = currResult
                                    
fact :: Int -> Integer
fact n = prodNgtM n 1

comb :: Int -> Int -> Integer
comb n m
  | n > m + m = let m' = n - m
                in prodNgtM n m' `div` fact (n - m')
  | otherwise = prodNgtM n m `div` fact (n - m)

perm :: Int -> Int -> Integer
perm n m = prodNgtM n (n - m)

risingFactPower :: Integer -> Int -> Integer
risingFactPower n m
  | m < 0 = error "Out of range parameter for risingFactPower."
  | otherwise = (product . take m) [n, n+1..]

fallingFactPower :: Integer -> Int -> Integer
fallingFactPower n m
  | m < 0 = error "Out of range parameter for fallingFactPower."
  | otherwise = (product . take m) [n, n-1..]

-- If you use an expression like 21 ^ 7 directly with the compiler flag -Wall, it will generate
-- a warning because it doesn't know what type the exponent is. These prevent that warning.

powIntegerIntNoWarn :: Integer -> Int -> Integer
powIntegerIntNoWarn x y = x ^ y

powInt64IntNoWarn :: Int64 -> Int -> Int64
powInt64IntNoWarn x y = x ^ y

-- Return the original number, the total number of digits before 0 or repeat, the number of digits
-- in a repeating cycle in the reciprical (0 for non-repeating, and the digits before a repeat.

type RepeatsRecipT = (Int, Int, Int, [Int])
repeatsInReciprocal :: Int -> RepeatsRecipT
repeatsInReciprocal value = repeatsInReciprocal' 10 0 [] (M.singleton 10 0)
  where
    repeatsInReciprocal' :: Int -> Int -> [Int] -> M.Map Int Int -> RepeatsRecipT
    repeatsInReciprocal' dividend count digits priorMap
      | dividend == 0 = (value, count, 0, reverse digits)
      | isNothing remainderLookup
        = repeatsInReciprocal' nextDividend newCount (quotient:digits)
          (M.insert nextDividend newCount priorMap)
      | otherwise = (value, newCount, newCount - fromJust remainderLookup, reverse newDigits)
      where
        (quotient, remainder) = dividend `quotRem` value
        nextDividend = remainder * 10
        newCount     = count + 1
        newDigits    = quotient : digits
        remainderLookup = M.lookup nextDividend priorMap

-- Identify first through fourth order patterns.
-- Generate first through fourth order sequences.
-- If found, True along with the initial values and the order increments are returned.
-- If it is not a patter, then False along with zeros is returned.
--
-- For example:
-- First order:  (True, 1, 2)           [1, 3, 5, 7]
-- Second order: (True, 1, 2, 1)        [1, 3, 6, 10]
-- Third order:  (True, 1, 2, 2, 1)     [1, 3, 7, 14, 25]
-- Fourth order: (True, 1, 2, 2, 2, 1)  [1, 3, 7, 15, 30, 56, 98]

isFirstOrderPat :: [Integer] -> (Bool, Integer, Integer)
isFirstOrderPat sequence1
  | null (drop 2 sequence1) = error "Input sequence for isFirstOrderPat must be 3 or more long."
  | allDiffsSame = (True, head sequence1, firstDiff)
  | otherwise = (False, 0, 0)
  where
    allDiffsSame = all (== firstDiff) diffs
    firstDiff = head diffs
    diffs = zipWith (-) (drop 1 sequence1) sequence1
    
genFirstOrderSeq :: Integer -> Integer -> [Integer]
genFirstOrderSeq val skip = val : genFirstOrderSeq (val + skip) skip

isSecondOrderPat :: [Integer] -> (Bool, Integer, Integer, Integer)
isSecondOrderPat sequence1
  | null (drop 3 sequence1) = error "Input sequence for isSecondOrderPat must be 4 or more long."
  | allDiffs2Same = (True, head sequence1, head diffs1, secDiff)
  | otherwise = (False, 0, 0, 0)
  where
    allDiffs2Same = all (== secDiff) diffs2
    secDiff = head diffs2
    diffs2 = zipWith (-) (drop 1 diffs1)  diffs1
    diffs1 = zipWith (-) (drop 1 sequence1) sequence1
    
genSecondOrderSeq :: Integer -> Integer -> Integer -> [Integer]
genSecondOrderSeq val skip secondarySkp
  = val : genSecondOrderSeq (val + skip) (skip + secondarySkp) secondarySkp

isThirdOrderPat :: [Integer] -> (Bool, Integer, Integer, Integer, Integer)
isThirdOrderPat sequence1
  | null (drop 4 sequence1) = error "Input sequence for isThirdOrderPat must be 5 or more long."
  | allDiffs3Same = (True, head sequence1, head diffs1, head diffs2, thirdDiff)
  | otherwise = (False, 0, 0, 0, 0)
  where
    allDiffs3Same = all (== thirdDiff) diffs3
    thirdDiff = head diffs3
    diffs3 = zipWith (-) (drop 1 diffs2)  diffs2
    diffs2 = zipWith (-) (drop 1 diffs1)  diffs1
    diffs1 = zipWith (-) (drop 1 sequence1) sequence1
    
genThirdOrderSeq :: Integer -> Integer -> Integer -> Integer -> [Integer]
genThirdOrderSeq val skip secondarySkp tertiarySkp
  = val : genThirdOrderSeq (val + skip) (skip + secondarySkp) (secondarySkp + tertiarySkp)
          tertiarySkp

isFourthOrderPat :: [Integer] -> (Bool, Integer, Integer, Integer, Integer, Integer)
isFourthOrderPat sequence1
  | null (drop 5 sequence1) = error "Input sequence for isFourthOrderPat must be 6 or more long."
  | allDiffs4Same = (True, head sequence1, head diffs1, head diffs2, head diffs3, fourthDiff)
  | otherwise = (False, 0, 0, 0, 0, 0)
  where
    allDiffs4Same = all (== fourthDiff) diffs4
    fourthDiff = head diffs4
    diffs4 = zipWith (-) (drop 1 diffs3)  diffs3
    diffs3 = zipWith (-) (drop 1 diffs2)  diffs2
    diffs2 = zipWith (-) (drop 1 diffs1)  diffs1
    diffs1 = zipWith (-) (drop 1 sequence1) sequence1
    
genFourthOrderSeq :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
genFourthOrderSeq val skip secondarySkp tertiarySkp quartSkp
  = val : genFourthOrderSeq (val + skip) (skip + secondarySkp) (secondarySkp + tertiarySkp)
          (tertiarySkp + quartSkp) quartSkp

-- Find the min or max of a single-parameter function within the range of input values given. The
-- function needs to be something like a parabola, where for example with min, if you were to pour
-- water into it, it would all settle to the bottom. It takes the function, the range, the number of
-- iterations to use (this later could be augmented to use an epsilon value), and a boolean
-- indicating whether to calculate the two intermediate values in parallel. The technique it uses is
-- to split the range at one-third and two-thirds, compute the function at those two points, then
-- iterate on the range (low, twoThirds) or (oneThird, high). I added the boolean for min or max,
-- but the variable names reflect the min case.

data MinMaxT = FindMin | FindMax deriving (Eq)

findMinOrMax :: (Floating a, Ord a) => MinMaxT -> (a -> a) -> (a, a) -> Int -> Bool -> (a, a)
findMinOrMax minMax fn (lowR, highR) iterCount parallel = resultPair
  where
    minMaxFn = if minMax == FindMin then (<=) else (>=)
    resultPair = iter iterCount (lowR, fn lowR) (highR, fn highR)
    iter 0 (low, _) (high, _) = let midpoint = (low + high) / 2
                                    fnMidpoint = fn midpoint
                                in fnMidpoint `seq` (midpoint, fnMidpoint)
    iter currCount lows@(low, _) highs@(high, _)
      | minMaxFn oneThirdResult twoThirdResult = iter newCount lows (twoThirdVal, twoThirdResult)
      | otherwise = iter newCount (oneThirdVal, oneThirdResult) highs
      where
        newCount = currCount - 1
        diff = high - low
        oneThirdDiff = diff / 3
        oneThirdVal = low + oneThirdDiff
        twoThirdVal = oneThirdVal + oneThirdDiff
        (oneThirdResult, twoThirdResult)
          | parallel  = (fn oneThirdVal, fn twoThirdVal) `using` parTuple2 rseq rseq
          | otherwise = (fn oneThirdVal, fn twoThirdVal)

-- Sum the list, doing a rem by the remVal when needed.

sumWithRem :: (Integral a) => a -> [a] -> a
sumWithRem remVal = foldl' addAndRemIfNeeded 0
  where
    addAndRemIfNeeded acc val = let newAcc = acc + val
                                in if newAcc < remVal then newAcc
                                   else newAcc `rem` remVal

-- Product the list, doing a rem by the remVal when needed.

prodWithRem :: (Integral a) => a -> [a] -> a
prodWithRem remVal = foldl' addAndRemIfNeeded 1
  where
    addAndRemIfNeeded acc val = let newAcc = acc * val
                                in if newAcc < remVal then newAcc
                                   else newAcc `rem` remVal

-- Functions for generating and manipulating state machines.

type StateMap k a = M.Map k (Int, Bool, [(a, Int)])

-- Generate a state map keyed off of an indicator of the state. The value associated with each key
-- is a triple containing a stateID starting at 0 for the initial state, a boolean indicating
-- whether this is an accept state, and a list of symbols from this point and the next state for
-- each.

genStateMap :: (Ord k) => [a] -> k -> (k -> Bool) -> ([a] -> k -> [a]) -> (a -> k -> k)
               -> StateMap k a
genStateMap alphabet initialState isFinalStateFn alphabetModFn addSymbolFn
  = let (_, _, fullStateMap) = gsm initialState (0, M.empty)
    in fullStateMap
  where
    gsm currState (nextAvailID, currMap)
      | isJust mapEntry = (correspondingID, nextAvailID, currMap)
      | otherwise = (nextAvailID, newNextAvailID, newMap)
      where
        mapEntry = M.lookup currState currMap
        (correspondingID, _, _) = fromJust mapEntry

        -- Replace the dummy entry with one with all of the transitions from here. Note that every
        -- state is a final state because we don't create states that aren't because once we have
        -- more than three of the same digit, we can't get back by adding digits.

        newMap = M.insert currState (nextAvailID, isFinalState, stateTransitsFromHere) mapAfterDFS
        isFinalState = isFinalStateFn currState

        -- If we need a new state for the current set of letters, allocate the next free ID for it,
        -- see if it is a final state (all needed letters seen), and create an entry without any
        -- further transitions. After we see what these transitions are and add to the map with the
        -- states reachable from a depth-first search from here, then go back and insert this again
        -- with those transitions. If we don't create this dummy first, then we can get in a loop.

        mapWithDummyKey = M.insert currState (nextAvailID, True, []) currMap
        nextAvailIDAfterThis = nextAvailID + 1

        -- Recursively call this function with each symbol added. Get the new map after this is done
        -- along with all of the transitions from here and the final next available ID.

        (newNextAvailID, mapAfterDFS, stateTransitsFromHere)
          = foldl' recCallForSymbol (nextAvailIDAfterThis, mapWithDummyKey, []) alphabetToUse

        -- There may be symbols that are not valid given the current state. For example, no zero as
        -- the first digit or can't use a digit more than 3 times. This function should weed out any
        -- symbols that don't work here.

        alphabetToUse = alphabetModFn alphabet currState

        -- Given a symbol add it to the current state and recursively call this function to add it
        -- and its downstream states to the map. Return the new map, new next ID and add the ID
        -- after adding this along with the current symbol to the transition list.

        recCallForSymbol (availID, inMap, accTrans) currSymbol = result
          where
            result = (newAvailID, nextMap, (currSymbol, corrID) : accTrans)
            (corrID, newAvailID, nextMap) = gsm stateWithThisSymbol (availID, inMap)
            stateWithThisSymbol = addSymbolFn currSymbol currState

-- The input is a state map where the value for each state is a triple (stateID, isFinal,
-- nextStates), where nextStates is a list of pairs (symbol, nextState). The key for the map is
-- ignored by this operation. This function will generate a vector where the index corresponds to
-- the stateID. The elements of the vector are made up of a boolean value indicating if this is a
-- final state or not, and a list of triples containing: (nextStateID, number of symbols going
-- there, and the list of symbols).

type StateVec a = V.Vector (Bool, [(Int, Int, [a])])

genStateVecFromStateMap :: StateMap k a -> StateVec a
genStateVecFromStateMap stateMap = correspondingStateVec
  where
    correspondingStateVec = V.fromListN (M.size stateMap) vecInitList

    -- Take the elements of the state map, as the key's are no longer needed, and sort them on the
    -- state ID, then map these into pairs with a Bool indicating whether this is a final state or
    -- not, and a collapsed list of transitions. Each of these transitions is a next state and the
    -- number of letters from the usable alphabet at that point that will go to that state.
    
    vecInitList = (map convToCounts . sortBy (compare `on` (\(c, _, _) -> c))
                   . map snd . M.toList) stateMap
    convToCounts (_, isFinal, symbolsAndNextStates) = (isFinal, nextStatesAndCounts)
      where
        nextStatesAndCounts = (map collapseSameNextStates . groupBy ((==) `on` snd)
                                . sortBy (compare `on` snd)) symbolsAndNextStates
        collapseSameNextStates xs = ((snd . head) xs, length xs, map fst xs)

-- Generate a state vector directly without explicitly generating a state map first. The same steps
-- are taken. This just saves the step of generating the state map first and then passing it to the
-- state vec function.

genStateVec :: (Ord k) => [a] -> k -> (k -> Bool) -> ([a] -> k -> [a]) -> (a -> k -> k)
               -> StateVec a
genStateVec alphabet initialState isFinalStateFn alphabetModFn addSymbolFn = stateVec
  where
    stateVec = genStateVecFromStateMap stateMap
    stateMap = genStateMap alphabet initialState isFinalStateFn alphabetModFn addSymbolFn

-- Holds a state and a count of the number of ways to get to that state.

data StateAndCount = StateAndCount { dfaState_r :: Int
                                   , dfaCount_r :: Integer
                                   } deriving (Show)

-- Return true if the given state and count record represents a final (accept) state.

isAcceptState :: StateVec a -> StateAndCount -> Bool
isAcceptState stateVec (StateAndCount currState _) = fst $ stateVec V.! currState

-- After a number of letters, the total set of states you could be in including the counts of ways
-- of getting to each of these states.

type StateSet = [StateAndCount]

-- Given a state transition vector, begin with a count of 1 at the start state (0), and work through
-- the counts for all possible moves through the state machine. Generate an infinite list of these
-- sets of states and counts, and the caller can do with them what they want.

statesAndCountsStartingAtState0 :: StateVec a -> [StateSet]
statesAndCountsStartingAtState0 transVec
  = iterate (genStatesAndCountsForOneMoreLett transVec) [StateAndCount 0 1]

-- Given the state transition vector and a set of current states and counts, generate the set of
-- states and counts after adding one more letter from the alphabet.

genStatesAndCountsForOneMoreLett :: StateVec a -> StateSet -> StateSet
genStatesAndCountsForOneMoreLett transVec currStatesAndCounts = newSAndC
  where

    -- Generate all states and counts with one additional alphabet letter added from the current
    -- state, then sort and group based on the state, then combine the counts from all identical
    -- states.

    newSAndC = (map combineCounts . groupBy ((==) `on` dfaState_r)
                . sortBy (compare `on` dfaState_r)
                . concatMap oneStepFromState) currStatesAndCounts

    -- Generate the states one step (alphabet letter) away from the given state, multiplying the
    -- current count by the number of alphabet letters resulting in this new state.

    oneStepFromState (StateAndCount st ct) = map multByCurrCount nextStates
      where
        nextStates = snd $ transVec V.! st
        multByCurrCount (s, c, _) = let newCount = fromIntegral c * ct
                                    in newCount `seq` StateAndCount s newCount

    -- Sum the counts for a set of identical sets.

    combineCounts [] = error "Null list to combineCounts."
    combineCounts stateList = StateAndCount theState combinedCount
      where
        theState = (dfaState_r . head) stateList
        combinedCount = (sum . map dfaCount_r) stateList

-- Begin at the start state, then iterate steps through the state vector accumulating counts at each
-- state after each step. After the given number of steps, sum the counts from the final states.

sumFinalCountsAfterNSteps :: StateVec a -> Int -> Integer
sumFinalCountsAfterNSteps stateVec nth = result
  where

    -- Sum the counts for all of the accept states for the given number of steps.

    result = (sum . map dfaCount_r . filter (isAcceptState stateVec)) statesAndCountsForLenN

    -- Get all of the states and counts after n steps through the state machine.

    statesAndCountsForLenN = statesAndCountsStartingAtState0 stateVec !! nth
