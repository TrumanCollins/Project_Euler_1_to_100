-- This module contains prime generation functions and data structures as well as several
-- prime-related functions.
--
-- By Truman Collins
-- December 21, 2015 ...

-- The following packages are needed to compile this:
-- cabal install vector
-- cabal install hashtables
-- cabal install mtl
-- cabal install containers
-- cabal install parallel

{-# Language FlexibleContexts #-}
{-# Language BangPatterns #-}
{-# Language MultiWayIf #-}

module PrimeMod (
  PrimeT,
  primesOneLine,
  myPrimes,
  myPrimesInt,
  myPrimesInt64,
  myPrimesInteger,
  myPrimesRational,
  myPrimesG,
  isPrime,
  isPrime64,
  isPrimeMR,
  millerRabinWitnessTest,
  myPrimeFactors,
  myPrimeFactorsInt,
  myPrimeFactorsInteger,
  myPrimeFactorsGroup,
  groupPrimeFactorsIntoPairs,
  removeFactors,
  hammingNumberInt,
  hammingNumberInt64,
  hammingNumberInteger,
  primesInRange,
  primesInRangeSegmented,
  primesInRangeSegmentedReversed,
  primeVector,
  primeVectorSize,
  primeVectorLimit,
  genPrimeIndexVec,
  genPrimeIndexVec32,
  initPrimeStorage,
  primeFnSumL,
  primeCount,
  primeCount64,
  primeSum,
  sumOfLeastPrimeFactorsBasic,
  sumOfLeastPrimeFactors,
  computePhiFromListOfPrimeFactors,
  computeTotient,
  computeCarmichaelFromListOfPrimeFactors,
  computeCarmichaelNumber,

  -- Accumulators for primeFactorsWalk

  accDivisorSumSq,
  accSumPhi,
  dfsPrimeToPFs,
  dfsGenDivisors,
  dfsGenDivisorCount,
  calculateDivisorCount,
  dfsGenPhi,
  
  primeFactorWalk,
  primeFactorWalkPar,
  visitValuesRec,
  listDFSWalk,
  listDFSWalk2
) where

import Data.Int
import Data.Bits
import Data.List
import Data.Maybe
import Data.Ratio
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.HashTable.Class as HT
import qualified Data.HashTable.ST.Cuckoo as HTC
import qualified Data.Map.Strict as MS
import Control.Monad.ST
import Control.Monad.State.Lazy
import Control.Parallel.Strategies
import Debug.Trace

-- Debug printing.

_primeCountComputationDebug :: Bool
_primeCountComputationDebug = False
_primeCountLookupDebug :: Bool
_primeCountLookupDebug = False
_primeSumComputationDebug :: Bool
_primeSumComputationDebug = False
_primeSumLookupDebug :: Bool
_primeSumLookupDebug = False

-- This function takes in a boolean, which will generally be statically defined in the module, a
-- trace string, and a result value.  If the boolean is true, the trace string will be printed and
-- the result returned, and if the boolean is false, then just the result will be returned with no
-- trace printed.

_debugTrace :: Bool -> String -> a -> a
_debugTrace False _ result = result
_debugTrace _ traceString result = trace traceString result

-- Define the Hastable type to use Cuckoo hashing.

type HashTable s k v = HTC.HashTable s k v

-- Compute the integer square root iteratively. Use Integer so we don't overflow with n^2.
-- This code is copied from EulerUtil.hs because of a circular dependency that I don't want to
-- create a new file to fix.

isqrt :: Integer -> Integer
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x * x > n)
               $ iterate (\x -> (x + n `quot` x) `quot` 2) (n `quot` 2)
isqrt64 :: Int64 -> Int64
isqrt64 n = fromIntegral $ isqrt (fromIntegral n)

-- This is a way of generating all of the primes in a very compressed piece
-- of code, but it's quite inefficient.

primesOneLine :: [Int64]
primesOneLine = sieve [2..]
  where
    sieve [] = []  -- Never used but it makes the compiler happy.
    sieve (p : xs) = p : sieve [x | x <- xs, x `rem` p > 0]

-- Generate an infinite list of primes.
-- Uses optimized trial division, and is quite fast.
-- October 6, 2022: reduced laziness saving memory and increasing speed.

type PrimeT     = Int
type PrimeSqT   = Int64
type PrimePairT = (PrimeT, PrimeSqT)

myPrimes :: [PrimeT]
myPrimes = 2 : 3 : 5 : 7 : map fst myPrimes'
  where
    myPrimes' :: [PrimePairT]
    myPrimes' = (11, 121) : sieve 13 myPrimes' skips
    sieve :: PrimeT -> [PrimePairT] -> [PrimeT] -> [PrimePairT]
    sieve _ [] _ = []
    sieve _ _ [] = []
    sieve potentialPrime ((prime, primeSq) : ps) allSkips@(skipHead : skipRest)
      | primeSq > potentialPrime_T
        = let potentialPrimeSq = nextPotentialPrime `seq` potentialPrime_T * potentialPrime_T
              headPair = potentialPrimeSq `seq` (potentialPrime, potentialPrimeSq)
          in  headPair `seq` headPair : sieve nextPotentialPrime myPrimes' skipRest
      | potentialPrime `rem` prime == 0
        = nextPotentialPrime `seq` sieve nextPotentialPrime myPrimes' skipRest
      | otherwise = sieve potentialPrime ps allSkips
      where
        nextPotentialPrime = potentialPrime + skipHead
        potentialPrime_T   = fromIntegral potentialPrime
    skips :: [PrimeT]
    skips = cycle [4, 2, 4, 6, 2, 6, 4, 2, 4, 6, 6, 2, 6, 4, 2, 6, 4, 6, 8, 4, 2,
                   4, 2, 4, 8, 6, 4, 6, 2, 4, 6, 2, 6, 6, 4, 2, 4, 6, 2, 6, 4, 2,
                   4, 2, 10, 2, 10, 2]

-- Prime list converted to Int, Int64, and Integer

myPrimesInt     :: [Int]
myPrimesInt     = map fromIntegral myPrimes
myPrimesInt64   :: [Int64]
myPrimesInt64   = map fromIntegral myPrimes
myPrimesInteger :: [Integer]
myPrimesInteger = map fromIntegral myPrimes
myPrimesRational :: [Rational]
myPrimesRational = map (% 1) myPrimesInteger
myPrimesG :: (Integral a) => [a]
myPrimesG = map fromIntegral myPrimes

-- Generate all prime factors of the given number grouped by count and ordered by prime.
-- For example: myPrimeFactorsGroup 

myPrimeFactorsGroup :: (Integral a) => a -> [(Int, a)]
myPrimeFactorsGroup 0 = []
myPrimeFactorsGroup x = (groupPrimeFactorsIntoPairs . reverse . pFs x myPrimesG) []
  where
    pFs _ [] result = result
    pFs targNumber (prime : restPrimes) result
      | targNumber == 1 = result
      | pSq > targNumber = targNumber : result
      | targNumber `rem` fromIntegral prime == 0 
        = pFs newTargetNumber (prime : restPrimes) (prime : result)
      | otherwise = pFs targNumber restPrimes result
      where
        pSq = fromIntegral prime * fromIntegral prime
        newTargetNumber = targNumber `quot` fromIntegral prime

-- Group prime factors into pairs containing the number of prime factors and the factor.
-- For example [5, 5, 5, 3, 3, 2, 2, 2] -> [(3, 5), (2, 3), (3, 2)]
-- This function is to be used specifically on the output of one of the myPrimeFactors functions.

groupPrimeFactorsIntoPairs :: (Eq a) => [a] -> [(Int, a)]
groupPrimeFactorsIntoPairs = map (\l -> (length l, head l)) . group

-- Remove all factors listed from the number, and return the remainder.
-- For example: removeFactors [2,5] 1300 = 13

removeFactors :: (Integral a) => [a] -> a -> a
removeFactors [] x = x
removeFactors factors@(f : fs) x
  | x `rem` f == 0 = removeFactors factors (x `quot` f)
  | otherwise = removeFactors fs x

-- Generate a list of the prime factors of the given number.  Uses myPrimes, and accepts an Int64.
    
myPrimeFactors :: Int64 -> [PrimeT]
myPrimeFactors 0 = []
myPrimeFactors targetNumber = primeFactors' targetNumber myPrimes []
  where
    primeFactors' :: Int64 -> [PrimeT] -> [PrimeT] -> [PrimeT]
    primeFactors' _ [] result = result
    primeFactors' targNumber (prime : restPrimes) result
      | targNumber == 1 = result
      | (fromIntegral prime * fromIntegral prime) > targNumber = fromIntegral targNumber : result
      | targNumber `rem` fromIntegral prime == 0 
        = primeFactors' newTargetNumber (prime : restPrimes) (prime : result)
      | otherwise = primeFactors' targNumber restPrimes result
      where
        newTargetNumber = targNumber `quot` fromIntegral prime

-- Generate a list of the prime factors of the given number, where it is an int.  This is much
-- faster than the one that takes an Int64.  Uses myPrimes.
    
myPrimeFactorsInt :: Int -> [PrimeT]
myPrimeFactorsInt 0 = []
myPrimeFactorsInt targetNumber = primeFactors' targetNumber myPrimes []
  where
    primeFactors' :: Int -> [PrimeT] -> [PrimeT] -> [PrimeT]
    primeFactors' _ [] result = result
    primeFactors' targNumber (prime : restPrimes) result
      | targNumber == 1 = result
      | (fromIntegral prime * fromIntegral prime) > targNumber = targNumber : result
      | targNumber `rem` fromIntegral prime == 0 
        = primeFactors' newTargetNumber (prime : restPrimes) (prime : result)
      | otherwise = primeFactors' targNumber restPrimes result
      where
        newTargetNumber = targNumber `quot` fromIntegral prime

-- Generate a list of the prime factors of the given number, where it is an int.  This is much
-- faster than the one that takes an Int64.  Uses myPrimes.
    
myPrimeFactorsInteger :: Integer -> [PrimeT]
myPrimeFactorsInteger 0 = []
myPrimeFactorsInteger targetNumber = primeFactors' targetNumber myPrimes []
  where
    primeFactors' :: Integer -> [PrimeT] -> [PrimeT] -> [PrimeT]
    primeFactors' _ [] result = result
    primeFactors' targNumber (prime : restPrimes) result
      | targNumber == 1 = result
      | (fromIntegral prime * fromIntegral prime) > targNumber = fromIntegral targNumber : result
      | targNumber `rem` fromIntegral prime == 0 
        = primeFactors' newTargetNumber (prime : restPrimes) (prime : result)
      | otherwise = primeFactors' targNumber restPrimes result
      where
        newTargetNumber = targNumber `quot` fromIntegral prime

-- Returns true if this is a Hamming number, having only prime factors 5 or under.

hammingNumberInt :: Int -> Bool
hammingNumberInt x
  | x <= 6 = True
  | otherwise = (head . myPrimeFactorsInt) x <= 5

hammingNumberInt64 :: Int64 -> Bool
hammingNumberInt64 x
  | x <= 6 = True
  | otherwise = (head . myPrimeFactors) x <= 5

hammingNumberInteger :: Integer -> Bool
hammingNumberInteger x
  | x <= 6 = True
  | otherwise = (head . myPrimeFactorsInteger) x <= 5

-- Generate the list of primes within a given range by using a sieve to mark all of the composites
-- as false in a bool vector representing the full range, then walk through the bool vector and
-- return the primes. We only need to use a list of primes up to the square root of the high range
-- to generate these.

primesInRange :: (Int64, Int64) -> [Int64]
primesInRange (lowLimitIn, highLimit) = resultingPrimesList
  where

    -- We can ignore any number in the limit below 2, and we need to or it will mess up ranges.

    lowLimit = if lowLimitIn < 2 then 2 else lowLimitIn

    -- Figure out the primes to return by walking through the boolean vector indicating which
    -- numbers in the range are primes. Use foldr so that the list can be returned as needed.

    resultingPrimesList = UV.ifoldr accPrimeListInRange [] isPrimeVec
    accPrimeListInRange i isPrm acc
      | isPrm = (fromIntegral i + lowLimit) : acc
      | otherwise = acc

    -- A boolean vector initialized to all true, then updated for each composite in the range with
    -- false. Note that numbers in the range that are composite may be updated multiple times.

    isPrimeVec = UV.replicate (fromIntegral (highLimit - lowLimit + 1)) True
                   UV.// compositesInRange
    compositesInRange = map genFalse primeMultiplesInRange
    genFalse val = let index = fromIntegral (val - lowLimit) in index `seq` (index, False)
    primesUpToSqrtOfHighLimit = takeWhile (<= isqrt64 highLimit) myPrimesInt64
    primeMultiplesInRange = concatMap genMultiplesInRange primesUpToSqrtOfHighLimit

    -- Generate a list of all of the multiples of the given prime in the given range. Make sure the
    -- low multiple is at least 2 or we will wipe out the prime itself if it is in the range.

    genMultiplesInRange p = foldr (:) [] [lowPrimeMult, (lowPrimeMult + p)..highPrimeMult]
      where
        lowPrimeMultTry  = ((lowLimit `quot` p) + (if lowLimit `rem` p == 0 then 0 else 1)) * p
        lowPrimeMult = if lowPrimeMultTry == p then p + p else lowPrimeMultTry
        highPrimeMult = (highLimit `quot` p) * p

primesInRangeDir :: Bool -> (Int64, Int64) -> [Int64]
primesInRangeDir ascendingOrder (lowLimitIn, highLimit) = resultingPrimesList
  where

    -- We can ignore any number in the limit below 2, and we need to or it will mess up ranges.

    lowLimit = if lowLimitIn < 2 then 2 else lowLimitIn

    -- Figure out the primes to return by walking through the boolean vector indicating which
    -- numbers in the range are primes. Use foldr so that the list can be returned as needed.

    resultingPrimesList = if ascendingOrder then UV.ifoldr accPrimeListInRange [] isPrimeVec
                          else foldr accPrimeListDescending [] [maxInd, maxInd - 1..0]
    maxInd = UV.length isPrimeVec - 1
    accPrimeListDescending i acc
      | isPrimeVec UV.! i = let p = fromIntegral i + lowLimit in p `seq` p : acc
      | otherwise = acc
    accPrimeListInRange i isPrm acc
      | isPrm = (fromIntegral i + lowLimit) : acc
      | otherwise = acc

    -- A boolean vector initialized to all true, then updated for each composite in the range with
    -- false. Note that numbers in the range that are composite may be updated multiple times.

    isPrimeVec = UV.replicate (fromIntegral (highLimit - lowLimit + 1)) True
                   UV.// compositesInRange
    compositesInRange = map genFalse primeMultiplesInRange
    genFalse val = let index = fromIntegral (val - lowLimit) in index `seq` (index, False)
    primesUpToSqrtOfHighLimit = takeWhile (<= isqrt64 highLimit) myPrimesInt64
    primeMultiplesInRange = concatMap genMultiplesInRange primesUpToSqrtOfHighLimit

    -- Generate a list of all of the multiples of the given prime in the given range. Make sure the
    -- low multiple is at least 2 or we will wipe out the prime itself if it is in the range.

    genMultiplesInRange p = foldr (:) [] [lowPrimeMult, (lowPrimeMult + p)..highPrimeMult]
      where
        lowPrimeMultTry  = ((lowLimit `quot` p) + (if lowLimit `rem` p == 0 then 0 else 1)) * p
        lowPrimeMult = if lowPrimeMultTry == p then p + p else lowPrimeMultTry
        highPrimeMult = (highLimit `quot` p) * p

-- Generate the primes in the given range, doing the work in the given number of segments. This is
-- used to generate primes in a large range where the boolean vector needed for the range is too
-- large.

primesInRangeSegmented :: Int64 -> (Int64, Int64) -> [Int64]
primesInRangeSegmented segSize (lowLimit, highLimit) = listOfPrimes
  where
    listOfPrimes = concatMap primesInRange segmentRanges
    segmentRanges = [(low, min highLimit (low + segSize - 1))
                     | low <- [lowLimit,(lowLimit + segSize)..highLimit]]
                                                 
primesInRangeSegmentedReversed :: Int64 -> (Int64, Int64) -> [Int64]
primesInRangeSegmentedReversed segSize (lowLimit, highLimit) = listOfPrimes
  where
    listOfPrimes = concatMap (primesInRangeDir False) segmentRanges
    segmentRanges = reverse [(low, min highLimit (low + segSize - 1))
                             | low <- [lowLimit,(lowLimit + segSize)..highLimit]]
                                                 
-- Store boolean flags for a lot of the lower primes so we can check primality quickly.  The size of
-- this boolean vector is coordinated with the size of the prime vector created below.

sizeOfPrimeQuickCheck :: Int
sizeOfPrimeQuickCheck = primeVectorLimit
primeNumberList :: [(Int, Bool)]
primeNumberList = zip (takeWhile (< sizeOfPrimeQuickCheck) myPrimes) (repeat True)
quickPrimeCheckV :: UV.Vector Bool
quickPrimeCheckV = UV.replicate sizeOfPrimeQuickCheck False UV.// primeNumberList

-- Create and return a vector with a max index the limit passed in. For any index that is prime, it
-- will hold the zero-based position of that prime in an ordered list of primes. For all non-prime
-- indexes the vector will contain -1. For example, if the limit passed in is 6, then the returned
-- vector will contain [-1, -1, 0, 1, -1, 2, -1].
    
genPrimeIndexVec :: Int -> UV.Vector Int
genPrimeIndexVec limit = UV.accum (+) (UV.replicate (limit + 1) (-1))
                                  (zip (takeWhile (<= limit) myPrimesInt) [1..])

genPrimeIndexVec32 :: Int -> UV.Vector Int32
genPrimeIndexVec32 limit = UV.accum (+) (UV.replicate (limit + 1) (-1))
                                  (zip (takeWhile (<= limit) myPrimesInt) [1..])

-- Determine if the number is prime.  If it is small enough, the boolean vector will be used to
-- check very quickly, otherwise we walk through primes looking for a factor.

isPrime :: Int -> Bool
isPrime val
  | val < 2 = False
  | val < sizeOfPrimeQuickCheck = quickPrimeCheckV UV.! val
  | otherwise = isPrime' myPrimes
  where
    isPrime' :: [PrimeT] -> Bool
    isPrime' [] = True
    isPrime' (p:ps)
      | p * p > fromIntegral val = True
      | fromIntegral val `rem` p == 0 = False
      | otherwise = isPrime' ps

isPrime64 :: Int64 -> Bool
isPrime64 val
  | val < 2 = False
  | val < fromIntegral sizeOfPrimeQuickCheck = quickPrimeCheckV UV.! fromIntegral val
  | otherwise = isPrime' myPrimes
  where
    isPrime' :: [PrimeT] -> Bool
    isPrime' [] = True
    isPrime' (p:ps)
      | p64 * p64 > val = True
      | val `rem` p64 == 0 = False
      | otherwise = isPrime' ps
      where p64 = fromIntegral p

-- Return true if the input is prime, using the Miller-Rabin Primality Test.  This uses a
-- deterministic version of the test where all primes below a certain limit are used, based on the
-- size of the input. It will correctly test all numbers up to 3317044064679887385961981.
-- Use the quickPrimeCheck vector if the prime is small enough. This is faster, but if the
-- prime check vector hasn't been created yet, then there is that added time.

isPrimeMR :: Integer -> Bool
isPrimeMR n
  | n <= 1 = False
  | n < fromIntegral sizeOfPrimeQuickCheck = quickPrimeCheckV UV.! fromIntegral n
--  | n <= 3 = True  -- Replace the prior line with this one to avoid using the quick check vec.
  | otherwise = (not . any (millerRabinWitnessTest n)) primesToCheck
  where

    -- Take the number of first primes needed to make sure we get the right answer, except in the
    -- otherwise case, where we choose a large number that should only fail in the very rarest of
    -- cases on very large numbers.

    primesToCheck = take numberOfPrimesToUse myPrimesInteger

    -- The number of primes starting at 2 that we need to check the primality of numbers up to the
    -- limit. Note that some jump two or three. The otherwise value is used for anything larger, and
    -- should be good, but it's still possible that the test would falsely identify a prime.
    -- These numbers came from The Online Encyclopedia of Integer Sequences, this being: A014233.

    numberOfPrimesToUse = if | n < 2047                      -> 1
                             | n < 1373653                   -> 2
                             | n < 25326001                  -> 3
                             | n < 3215031751                -> 4
                             | n < 2152302898747             -> 5
                             | n < 3474749660383             -> 6
                             | n < 341550071728321           -> 8
                             | n < 3825123056546413051       -> 11
                             | n < 318665857834031151167461  -> 12
                             | n < 3317044064679887385961981 -> 13
                             | otherwise                     -> 50

    
-- Returns true if a is a witness to the composite-ness of n.

millerRabinWitnessTest :: Integer -> Integer -> Bool
millerRabinWitnessTest n a

  -- Test for valid a, and indirectly that n > 3.

  | a <= 1 || a >= (n - 1) = error $ "Witness out of range for n (n: "
                                     ++ show n ++ ", witness: " ++ show a ++ ")"

  -- If n is even, then all numbers a are implicit witnesses that n is composite.

  | even n = True

  -- If the initial x value is 1, then no need to look further. This a is not a witness to a
  -- composite n. Note that most algorithms check for x == (n - 1) here too, but I include it in the
  -- otherwise test, which simplifies.

  | x == 1 = False

  -- Square x (twoPower - 1) times and test this list for (n - 1), which indicates no need to look
  -- further for a witness to n being composite. If we get to the end of this list without seeing an
  -- (n - 1), then we have a witness, so return True.

  | otherwise = foldr (\val final -> (val /= nMinus1) && final) True successiveSquares
  where
    nMinus1 = n - 1

    -- The list of values following (and including) x found by squaring.

    successiveSquares = (take (twoPower + 1) . iterate (\y -> y * y `rem` n)) x

    -- The initial value to check for x == 1 and x == (n - 1).

    x = modularExp a nonTwoFactorComp n
    nonTwoFactorComp = nMinus1 `shiftR` twoPower

    -- The number of two-factors of n.

    twoPower = (length . takeWhile even . iterate (`shiftR` 1)) nMinus1

    -- Fast modular exponentiation.  Use a method where we square the base each loop, and multiply
    -- it into the result for each bit set in the exponent. Note that this code is replicated here
    -- to avoid a circular dependency with EulerUtil.hs
    
    modularExp :: (Integral a, Bits a) => a -> a -> a -> a
    modularExp base expon modulus = modular_exp 1 (base `rem` modulus) expon
      where
        modular_exp currResult currBase currExp
          | currExp == 0 = currResult
          | currExp .&. 1 == 0 = modular_exp currResult newBase newExp
          | otherwise = let newResult = (currResult * currBase) `rem` modulus
                        in newResult `seq` modular_exp newResult newBase newExp
          where
            newExp = shiftR currExp 1
            newBase = (currBase * currBase) `rem` modulus

-- A vector containing the primes up to 1,000,000.

primeVectorLimit :: Int
primeVectorLimit = 1000000
primeVectorSize :: Int
primeVectorSize = UV.length primeVector
primeVector :: UV.Vector PrimeT
primeVector = UV.fromList (takeWhile (<= primeVectorLimit) myPrimes)

-- Insure that the primeVector is allocated and filled, as is the bit vector associated with
-- isPrime.  Take the last prime listed in the vector, the number just before it and the number just
-- after it, and insure that the former registeres as prime from the isPrime function, and that
-- latter two do not.  Return a string indicating what we did and if there was an error or not.

initPrimeStorage :: IO String
initPrimeStorage = do
  let lastPrimeInVector = primeVector UV.! (primeVectorSize - 1)
      shouldBePrime = isPrime lastPrimeInVector
      shouldNotBePrime1 = isPrime (lastPrimeInVector - 1)
      shouldNotBePrime2 = isPrime (lastPrimeInVector + 1)
      errorText = if shouldBePrime && not shouldNotBePrime1 && not shouldNotBePrime2
                  then ""
                  else "  Error: isPrime test broken."
  return ("Generated first " ++ show primeVectorSize ++ " primes up to "
          ++ show primeVectorLimit ++ errorText)

-- This function will return the sum of f(p) for all p up to each of the limits defined in
-- limitList, returning a list of the results in corresponding order. A hashtable is used for the
-- recurrence, and allowing a list of inputs means that each calculation can use the results in the
-- hashtable. Two functions are required: limFn is the sum of f(n) for [1..limit], primeFn is
-- f(). For example: to count the primes from 1 to n, call:
--   primeFnSumL (\x -> x * (x + 1) `quot` 2) id [n]

primeFnSumL :: (Integer -> Integer) -> (Integer -> Integer) -> [Integer] -> [Integer]
primeFnSumL _ _ [] = []
primeFnSumL limFn primeFn limitList = sumPrimesFns
  where

    -- Get the largest limit so we can create the maximum needed vector of primes up front and use
    -- it for all calculations.

    maxLimit = maximum limitList

    -- A vector of the primes up to the square root of the maximum limit. For smaller limits we will
    -- just use a slice of this vector of appropriate size.

    primeVec = V.fromList (1 : primesToSqrt)
      where
        primesToSqrt = takeWhile (<= sqrtLimit) myPrimesInteger

    -- The number of primes to that limit up to the square root of the maximum limit.
    -- Limit the entries in the hashtable to the fourth root of the limit.

    sqrtLimit = isqrt maxLimit

    primeCountToSqrt = V.length primeVec - 1

    -- Find the number of bits in the maximum prime index so we can merge the two key values for the
    -- hashtable into one Integer, saving space.

    bitsInMaxPrimeIndex = (length . takeWhile (/= 0) . iterate (`shiftR` 1)) primeCountToSqrt

    -- The maximum n to put in the hashtable. If we use too large a value, a lot of space is used
    -- for (n, i) pairs that will never be seen again and that slow down other searches.

    maxNForHashtable = 2000

    -- Use a hashtable to sum the following recurrence for D(limit, primeCountToSqrt) given
    -- a vector of primes p[]:
    --   D(n, 0) = limitFn n
    --   D(n, i) = D(n, i-1) - (primeFn p[i]) * D(n/p[i], i-1)
    -- This simplifies to:
    --   D(n, i) = (limitFn n) - sum(k=1 to i) ((primeFn p[k]) * D(n/p[k], k - 1))
    -- Because a hashtable runs in ST and I don't want to pass it around to each recurrent call, use
    -- two levels of function within a function. Merge the value and prime index into a single
    -- Integer for the hash key, which doesn't make much difference for runtime, but uses less
    -- memory.

    sumPrimesFns :: [Integer]
    sumPrimesFns = runST $
      do
        hashT <- HT.new
        resultList <- sumPrimesFnsHT hashT
        return (reverse resultList)
      where

        -- This function exists to take in the hashtable so it is in scope, then call an inner
        -- function with the limit.

        sumPrimesFnsHT :: HashTable s Integer Integer -> ST s [Integer]
        sumPrimesFnsHT hashTable = foldM computeSumOfFnToLimit [] limitList
          where

            computeSumOfFnToLimit accList currLimit
              | currLimit < 2  = return (0 : accList)
              | otherwise = do

                  -- The result of this function is the D function described above for the limit and
                  -- the number of primes up to the square root of the limit, which includes 1 and
                  -- doesn't include the f(p) of the primes up to the square root of the limit. We
                  -- subtract the 1 and add in the latter to get the full result. This is added to
                  -- the front of the accumulator.

                  let resultUpToSqrt = (sum . map primeFn . tail) (V.toList currPrimeVec)
                  resultFromRecurrence <- sumPrimesFnsRec currLimit currPrimeCountToSqrt
                  let result = resultFromRecurrence - 1 + resultUpToSqrt
                  return (result `seq` (result : accList))
              where

                -- Create a prime vector (no copy done) with the appropriate size for this limit.

                currSqrtLimit = isqrt currLimit
                currPrimeVec = V.takeWhile (<= currSqrtLimit) primeVec
                currPrimeCountToSqrt = V.length currPrimeVec - 1

                -- This is the recursive function that corresponds to D in the above recurrence.

                sumPrimesFnsRec n index
                  | n < 2 = return n

                  -- If the current prime is greater than or equal to n, then we only count 1.

                  | currPrime >= n = return 1

                  -- If the index is zero, then we want to sum for all n.

                  | index == 0 = let fullSum = limFn n
                                 in  fullSum `seq` return fullSum

                  -- If n is less than the square of the current largest prime, then we will get the
                  -- same answer with the prior prime with less work, although we have to subtract
                  -- (primeFn currPrime) because there would be one fewer prime counted with the
                  -- larger index.
                  -- I changed this to find the index and prime such that n >= (pr * pr) without
                  -- recursively calling this function for each smaller index, which made a huge
                  -- difference in memory usage.

                  | n < currPrime * currPrime = do
                        let newIndex = (fst . head . dropWhile primeStillTooBig
                                        . map iToIndexPrime) [1..]
                            sumPrimeFns = (sum . map genPrimeFnForPrimeI) [(newIndex + 1)..index]
                            primeStillTooBig (_, pr) = pr > 2 && n < (pr * pr)
                            iToIndexPrime i = let newI = index - i
                                                  newPr = primeVec V.! newI
                                              in newPr `seq` (newI, newPr)
                            genPrimeFnForPrimeI i = primeFn (primeVec V.! i)
                        valueWithLowerPrimeIndex <- sumPrimesFnsRec n newIndex
                        return (valueWithLowerPrimeIndex - sumPrimeFns)

                  -- Here we look up the current (n, i) pair in the hashtable and return it if a
                  -- value exists for it. If not, then compute:
                  --   D(n, i) = (limitFn n) - sum(k=1 to i) ((primeFn p[k]) * D(n/p[k], k - 1))
                  -- add it to the hashtable, and return it.

                  | otherwise = do
                        let hashKey = shiftL n bitsInMaxPrimeIndex .|. fromIntegral index
                        valMaybe <- doLookup n hashKey
                        case valMaybe of
                          Just val -> return val
                          Nothing -> do

                            -- Define the function to fold through the list of indexes to consider.

                            let sumForPrimes acc primeIndex = do
                                  let prm = primeVec V.! primeIndex
                                      nQPrm = n `quot` prm
                                      oneLessIndex = primeIndex - 1
                                  res <- sumPrimesFnsRec nQPrm oneLessIndex
                                  let fullRes = primeFn prm * res
                                  return (acc + fullRes)

                            -- Compute the result, add it to the hashtable if the n is small enough,
                            -- and return it.

                            resFromSum <- foldM sumForPrimes 0 [1..index]
                            let result = limFn n - resFromSum
                            when (result `seq` n <= maxNForHashtable)
                                 (HT.insert hashTable hashKey result)
                            return result
                  where
                    currPrime = primeVec V.! index

                    -- Slightly faster to check the size of n before doing the lookup.

                    doLookup n1 hashkey = do
                      if n1 > maxNForHashtable
                      then return Nothing
                      else HT.lookup hashTable hashkey

-- Count the primes up to limit, with a separate Int64 version.

primeCount :: Integer -> Integer
primeCount limit = head $ primeFnSumL id (const 1) [limit]

primeCount64 :: Int64 -> Int64
primeCount64 limit = (fromIntegral . head) (primeFnSumL id (const 1) [fromIntegral limit])

-- Sum the primes up to limit.

primeSum :: Integer -> Integer
primeSum limit = head $ primeFnSumL (\x -> x * (x + 1) `quot` 2) id [limit]

primeMultiplesAndCounts :: Integer -> Int -> (Integer, Integer)
primeMultiplesAndCounts _ 0 = (0, 0)
primeMultiplesAndCounts n m = (pM, pMC)
  where
    pM = priorPM + nDiv_mthPrime * (nDiv_mthPrime + 1) `quot` 2 * mthPrime
         - adjPM * mthPrime
    pMC = priorPMC + nDiv_mthPrime - adjPMC
    (priorPM, priorPMC) = primeMultiplesAndCounts n (m - 1)
    (adjPM, adjPMC) = primeMultiplesAndCounts nDiv_mthPrime (m - 1)
    mthPrime = fromIntegral $ primeVector UV.! zeroBasedIndex
    zeroBasedIndex = fromIntegral (m - 1)
    nDiv_mthPrime = n `quot` mthPrime

sumOfLeastPrimeFactorsUpToPrimeIndexBasic :: Integer -> Int -> Integer
sumOfLeastPrimeFactorsUpToPrimeIndexBasic n 0 = n * (n + 1) `quot` 2
sumOfLeastPrimeFactorsUpToPrimeIndexBasic n m
  | n < mthPrime * mthPrime = sumOfLeastPrimeFactorsUpToPrimeIndexBasic n (m - 1)
  | otherwise = computedResult
  where
    computedResult = priorSum - nDiv_mthPrime * (nDiv_mthPrime + 1) `quot` 2 * mthPrime
                     + nDiv_mthPrime * mthPrime + mthPrime * primeMults
                     - mthPrime * primeMultCount
    priorSum = sumOfLeastPrimeFactorsUpToPrimeIndexBasic n (m - 1)
    (primeMults, primeMultCount) = primeMultiplesAndCounts nDiv_mthPrime (m - 1)
    mthPrime = fromIntegral $ primeVector UV.! zeroBasedIndex
    zeroBasedIndex = fromIntegral (m - 1)
    nDiv_mthPrime = n `quot` mthPrime   
 
sumOfLeastPrimeFactorsBasic :: Integer -> Integer
sumOfLeastPrimeFactorsBasic 0 = 0
sumOfLeastPrimeFactorsBasic n = sumOfLeastPrimeFactorsUpToPrimeIndexBasic n primeInd - 1
  where
    primeInd = (fromIntegral . primeCount . fromIntegral . isqrt) n


primeMultiplesAndCountsMemo :: MS.Map Int64 Integer -> Integer -> Int
                               -> (Integer, Integer, MS.Map Int64 Integer)
primeMultiplesAndCountsMemo hashMap _ 0 = (0, 0, hashMap)
primeMultiplesAndCountsMemo hashMap n m
  | isJust lookupResult = let foundAll = fromJust lookupResult
                              foundPc  = foundAll .&. 0xFFFFFFFFF
                              foundPm  = shiftR foundAll 40
                          in (foundPm, foundPc, hashMap)
  | otherwise = (pM, pMC, MS.insert hashKey (shiftL pM 40 .|. pMC) hashMap1)
  where
    hashKey = fromIntegral (shiftL n 20 .|. fromIntegral m) :: Int64
    lookupResult = MS.lookup hashKey hashMap
    (!pM, !pMC, !hashMap1) = foldl' addValuesForThisPrime (0, 0, hashMap) [0..(m-1)]
    addValuesForThisPrime (accPM, accPMC, accHashMap) primeIndex = (newPM, newPMC, newHashMap)
      where
        !newPM  = accPM + nDivCurrPrime * (nDivCurrPrime + 1) `quot` 2 * currPrime - pMDiv * currPrime
        !newPMC = accPMC + nDivCurrPrime - pMCDiv
        (pMDiv, pMCDiv, newHashMap) = primeMultiplesAndCountsMemo accHashMap nDivCurrPrime primeIndex
        currPrime = fromIntegral (primeVector UV.! primeIndex)
        nDivCurrPrime = n `quot` currPrime

sumOfLeastPrimeFactorsUpToPrimeIndex :: Integer -> Int -> Integer
sumOfLeastPrimeFactorsUpToPrimeIndex n 0 = n * (n + 1) `quot` 2
sumOfLeastPrimeFactorsUpToPrimeIndex n m
  | n < mthPrime * mthPrime = sumOfLeastPrimeFactorsUpToPrimeIndex n (m - 1)
  | otherwise = computedResult
  where
    computedResult = n * (n + 1) `quot` 2 - sumOfIntermediaries
    mthPrime = fromIntegral $ primeVector UV.! zeroBasedIndex
    zeroBasedIndex = fromIntegral (m - 1)
    -- This was useful for debugging.
    --    (maxPM, maxPC) = MS.foldl' (\(accPM, accPC) currAll -> let !currPM = shiftR currAll 40
    --                                                               !currPC = currAll .&. 0xFFFFFFFFF
    --                                                               !newPM = if accPM > currPM then accPM else currPM
    --                                                               !newPC = if accPC > currPC then accPC else currPC
    --                                                           in (newPM, newPC)) (0, 0) hm
    (sumOfIntermediaries, _) = foldl' addValuesForThisPrime (0, MS.empty) [0..(m-1)]
    addValuesForThisPrime (accSum, accMemo) primeIndex = (newSum, newMemo)
      where
        !newSum = accSum + nDivCurrPrime * (nDivCurrPrime + 1) `quot` 2 * currPrime
                  - nDivCurrPrime * currPrime - currPrime * pMs + currPrime * pCs
        (pMs, pCs, !newMemo) = primeMultiplesAndCountsMemo accMemo nDivCurrPrime primeIndex
        nDivCurrPrime = n `quot` currPrime
        currPrime = fromIntegral (primeVector UV.! primeIndex)
 
sumOfLeastPrimeFactors :: Integer -> Integer
sumOfLeastPrimeFactors 0 = 0
sumOfLeastPrimeFactors n = sumOfLeastPrimeFactorsUpToPrimeIndex n primeInd - 1
  where
    primeInd = (fromIntegral . primeCount . fromIntegral . isqrt) n

-- Compute Totient of prime power.

computePrimePowerTotient :: (Int, PrimeT) -> Int64
computePrimePowerTotient (count, p) = let p64 = fromIntegral p
                                          firstVal = p64 - 1
                                      in if count == 1 then firstVal
                                         else firstVal * p64 ^ (count - 1)

-- Compute the Euler Totient Function (Phi) from a list of pairs of counts and prime numbers.  It is
-- assumed that the counts will be 1 or greater, and the primes are unique.

computePhiFromListOfPrimeFactors :: [(Int, PrimeT)] -> Int64
computePhiFromListOfPrimeFactors = foldl' (\acc pairCP -> acc * computePrimePowerTotient pairCP) 1

-- Compute Euler's Totient Function (Phi).

computeTotient :: Int64 -> Int64
computeTotient = computePhiFromListOfPrimeFactors . groupPrimeFactorsIntoPairs . myPrimeFactors

-- Compute the Carmichael number of a prime power.

computePrimePowerCarmichaelNumber :: (Int, PrimeT) -> Int64
computePrimePowerCarmichaelNumber pair@(count, p) = let totient = computePrimePowerTotient pair
                                                    in if p == 2 && count >= 3 then totient `quot` 2
                                                       else totient

-- Compute Carmichael's Function (Lambda) from a list of pairs of counts and prime numbers.  It is
-- assumed that the counts will be 1 or greater, and the primes are unique.

computeCarmichaelFromListOfPrimeFactors :: [(Int, PrimeT)] -> Int64
computeCarmichaelFromListOfPrimeFactors factors
  = foldl' lcm 1 (map computePrimePowerCarmichaelNumber factors)

-- Compute Carmichael's Number (Lambda).

computeCarmichaelNumber :: Int64 -> Int64
computeCarmichaelNumber = computeCarmichaelFromListOfPrimeFactors . groupPrimeFactorsIntoPairs
                          . myPrimeFactors

-- Here is a set of useful functions to pass to primeFactorWalk and primeFactorWalkPar.
-- The first group are for the main accumulator, and the second for the dfsAccumulator.

-- This accumulator works with dfsGenDivisors to sum all values whose divisors sum to a square.

accDivisorSumSq :: Int64 -> Int64 -> ((Int, PrimeT), [Int64]) -> Int64
accDivisorSumSq acc currVal ((cC, cP), divs)
  | sqrtSqrd == divisorSqSum = acc + currVal
  | otherwise = acc
  where
    sqrtSqrd = sqrtSum * sqrtSum
    sqrtSum = isqrt64 divisorSqSum
    divisorSqSum = (sum . map ((\x -> x * x) . fromIntegral)) divisors
    divisors = (*) <$> map (cPI ^) [0..cC] <*> divs
    cPI = fromIntegral cP

-- Add up the phi values.

accSumPhi :: Int64 -> Int64 -> (PrimeT, PrimeT) -> Int64
accSumPhi acc _ (_, currPhi) = acc + fromIntegral currPhi

-- A useful dfsAccum function for primeFactorsWalk, which will insure at each point that there is a
-- list of (count, prime) pairs indicating the prime factorization at this point in the walk.
-- The appropriate initial state is [].

dfsPrimeToPFs :: [(Int, PrimeT)] -> PrimeT -> [(Int, PrimeT)]
dfsPrimeToPFs [] p = [(1, p)]
dfsPrimeToPFs currFactors@((dC, dP) : dPCs) p
  | p == dP = let newCnt = dC + 1
                  newPair = newCnt `seq` (newCnt, dP)
              in newPair `seq` newPair : dPCs
  | otherwise = let newPair = (1, p)
                in newPair `seq` newPair : currFactors

-- This function will accumulate the list of divisors at the current dfs point. It will accumulate a
-- (count, prime) pair until we reach a new prime, then will generate a new list of divisors based
-- on the ones from before and the current pair.
-- An appropriate initial state is: ((0, 0), [1])

dfsGenDivisors :: ((Int, PrimeT), [Int64]) -> PrimeT -> ((Int, PrimeT), [Int64])
dfsGenDivisors ((cC, cP), divs) p
  | p == cP = let newCnt = cC + 1 in newCnt `seq` ((newCnt, cP), divs)
  | otherwise = ((1, p), (*) <$> map (cPI ^) [0..cC] <*> divs)
  where cPI = fromIntegral cP

-- This dfs accumulator function will keep track of the number of divisors. It has the helper
-- function calculateDivisorCount to use in the accumulator to compute the current divisor count.
-- An appropriate initial state is: ((0, 0), 1)

dfsGenDivisorCount :: ((Int, PrimeT), Int64) -> PrimeT -> ((Int, PrimeT), Int64)
dfsGenDivisorCount ((cC, cP), divC) p
  | p == cP = let newCnt = cC + 1 in newCnt `seq` ((newCnt, cP), divC)
  | otherwise = let newDivC = divC * (fromIntegral cC + 1)
                in newDivC `seq` ((1, p), newDivC)

calculateDivisorCount :: ((Int, PrimeT), Int64) -> Int64
calculateDivisorCount ((cC, _), divC) = divC * (fromIntegral cC + 1)

-- Keep track of the current Phi for the current value. An appropriate initial value is (0, 1).

dfsGenPhi :: (PrimeT, PrimeT) -> PrimeT -> (PrimeT, PrimeT)
dfsGenPhi (lastPrime, currPhi) currPrime
  | lastPrime == currPrime = let newPhi = currPhi * currPrime in newPhi `seq` (lastPrime, newPhi)
  | otherwise = let newPhi = currPhi * (currPrime - 1) in newPhi `seq` (currPrime, newPhi)

-- This function is based on primeFactorsWalkThroughSumPhi, and will visit each number from 2 to the
-- given limit (not in any particular order) calling an accumulator function for each one passing in
-- both the number and a list of pairs of prime factors and counts.  This can be used to flexibly do
-- something for each number based on its prime factors.  Note that a function is passed in to
-- potentially modify the primes list, by typically limiting the maximum prime.  If you want to
-- change the front of the prime list, call this function and not
-- primeFactorsWalkThroughAccFuncPar.  Also, a flag can be passed in to only visit even numbers.

primeFactorWalk :: Int64 -> a -> b -> (a -> Int64 -> b -> a) -> (b -> PrimeT -> b) -> Bool
                   -> ([PrimeT] -> [PrimeT]) -> a
primeFactorWalk limit accum dfsAccum accumFunc dfsAccumFunc onlyEven modPrmsFunc
  | onlyEven  = let initPrime = 2 :: Int64
                    initDfsAccum = dfsAccumFunc dfsAccum 2
                    initAccum = accumFunc accum initPrime initDfsAccum
                in visitValuesRec limit initPrime initAccum initDfsAccum accumFunc dfsAccumFunc
                                  (modPrmsFunc myPrimes)
  | otherwise = visitValuesRec limit 1 accum dfsAccum accumFunc dfsAccumFunc (modPrmsFunc myPrimes)

-- This is the same as primeFactorsWalkThroughAccFunc, but parallelizes the search.  It takes an
-- extra parameter, which is a function to merge two accumulators.  Note that this should only be
-- called when the accumulator passed in is 'empty' as it will be passed to all of the start
-- sub-tree searches, and if it's not empty, what is there will be included multiple times.

primeFactorWalkPar :: Int64 -> a -> b -> (a -> Int64 -> b -> a) -> (b -> PrimeT -> b)
                                  -> ([PrimeT] -> [PrimeT]) -> ([a] -> a) -> a
primeFactorWalkPar limit accum dfsAccum accumFunc dfsAccumFunc modPrmsFunc combineAccums
  | limit < 100 = primeFactorWalk limit accum dfsAccum accumFunc dfsAccumFunc False modPrmsFunc
  | otherwise = combineAccums resultList
  where

    -- Generate the results of the various branches of the search tree in parallel.

    resultList = parMap rpar callVisit listOfVisitStartStates

    -- Get a the list of start states for the parallel search of sub-trees.  These are based on
    -- different sets of small prime factors.

    listOfVisitStartStates = genListOfWalkStarts dfsAccum dfsAccumFunc modPrmsFunc

    -- Given a start state for a search sub-tree, call visitValsRec on it.

    callVisit (currProd, beginDfsAccum, prms)
      = visitValuesRec limit currProd beginAccum beginDfsAccum accumFunc dfsAccumFunc prms
      where beginAccum
              | currProd == 1 = accum
              | otherwise = accumFunc accum currProd beginDfsAccum

-- Generate the list of search start states for a parallel prime factor walk search. These will be
-- returned roughly in order of longest running to shortest so that running them in parallel in this
-- order will take best advantage of parallelism. The first one will be with an empty list of
-- factors starting at the largest prime, so as to hit and generate primes not yet found, and
-- because working with these larger primes can take more time because larger ones are needed with
-- no existing factors.

genListOfWalkStarts :: b -> (b -> PrimeT -> b) -> ([PrimeT] -> [PrimeT])
                                            -> [(Int64, b, [PrimeT])]
genListOfWalkStarts dfsAccum dfsAccumFn modPrmsFunc = listOfVisitStartStates
  where

    -- Generate the modified list of primes.  Note that the modifying function shouldn't change any
    -- of the first sixteen primes (up to 53).

    myPrimesModded = modPrmsFunc myPrimes

    -- Lists of primes starting at different initial primes.

    [prms2, prms3, prms5, prms7, prms11, prms13, prms17, prms19, prms23, prms29,
     prms31, prms37, prms41, prms43, prms47, prms53] = take 16 (iterate (drop 1) myPrimesModded)

    -- For each start state, there will be some number of factors included and a first prime to
    -- begin with adding. In all cases except when there is an empty factor list, the accumulator
    -- should be called.  The dfs accumulator should be called for each factor prior to the start.

    listOfVisitStartStates
      = [gs [] prms53, gs [2] prms31, gs [3] prms13, gs [2, 2] prms11, gs [2, 3] prms5,
         gs [5] prms7, gs [2, 2, 3] prms3, gs [2, 2, 2] prms5, gs [2, 2, 2, 2, 2] prms2,
         gs [3, 3] prms5, gs [7] prms7, gs [2, 2, 2, 2] prms3, gs [2, 5] prms5, gs [2, 3, 3] prms3,
         gs [2, 2, 2, 3] prms3, gs [3, 5] prms5, gs [3, 3, 3] prms3, gs [11] prms11,
         gs [2, 7] prms7, gs [2, 2, 5] prms5, gs [13] prms13, gs [5, 5] prms5, gs [3, 7] prms7,
         gs [17] prms17, gs [2, 2, 7] prms7, gs [2, 11] prms11, gs [19] prms19, gs [2, 13] prms13,
         gs [23] prms23, gs [3, 11] prms11, gs [2, 17] prms17, gs [29] prms29, gs [31] prms31,
         gs [2, 19] prms19, gs [37] prms37, gs [2, 23] prms23, gs [41] prms41, gs [43] prms43,
         gs [47] prms47, gs [2, 29] prms29]

    -- genStart (gs): Each start point is defined by: the initial starting number, the state of the
    -- dfsAccumulator at the starting point, and the list of primes to begin searching with.

    gs factors primeStart = startNum `seq` startDfsAccum `seq` (startNum, startDfsAccum, primeStart)
      where
        startNum = (fromIntegral . product) factors
        startDfsAccum = foldl' dfsAccumFn dfsAccum factors

-- Used by primeFactorWalk and primeFactorWalkPar to do a depth-first search given a list of primes
-- and accumulator functions and values. Note that the initial value isn't checked. See listDFSWalk2.

visitValuesRec :: Int64 -> Int64 -> a -> b -> (a -> Int64 -> b -> a) -> (b -> PrimeT -> b)
                  -> [PrimeT] -> a
visitValuesRec limit currProduct finalAccum dfsAccum accumFn dfsAccumFn listOfPrimes
  = visitValsRec currProduct dfsAccum listOfPrimes finalAccum
  where

    -- Visit all values reachable multiplying the primes in the list keeping at or below limit.  We
    -- usually work with an infinite primes list, but a finite list can be used. This is the
    -- recursive function.

    visitValsRec _ _ [] accum = accum
    visitValsRec currProd currDfsAccum prms@(p : ps) accum

      -- If by multiplying the current product by the current prime gets us over the limit, then we
      -- have nothing more to do in this line of the search tree, so return the passed in
      -- accumulator. While we can be over the limit by a 'small' amount, it won't be by too much
      -- because we know that the previous prime multiplied by currProduct was under the limit, from
      -- the caller.

      | newProd > limit = accum

      -- If we would be over the limit by multiplying again by the current prime (or any larger
      -- prime), update the accumulator and pass it to a further search ignoring this prime.  There
      -- are cases where by not doing this check and relying on the test above, we would overflow,
      -- plus it is a bit faster to check here.  Note that we are checking whether currProduct * p *
      -- p is over the limit, and if it is, we are calling recursively to check currProduct * nextP,
      -- so while it might also be over the limit, it won't be by a huge amount given that
      -- currProduct * p wasn't.

      | newAcc `seq` fromIntegral p > (limit `quot` newProd) = visitNextPrimeInsteadOfThis newAcc

      -- In the usual case, update the accumulator, recursively call with larger primes not using
      -- the current one again, then take the accumulator returned and pass it on to further use
      -- the current prime.

      | otherwise = let accNextPrime = visitNextPrimeInsteadOfThis newAcc
                    in  accNextPrime `seq` visitThisPrimeAgain accNextPrime
      where

        -- Compute the new product given this prime and the new accumulator with it too. Note that
        -- statically computing the new DFS accumulator makes a real difference here for both time
        -- and space.

        newProd = currProd * fromIntegral p
        newAcc  = newDfsAccum `seq` accumFn accum newProd newDfsAccum

        -- Generate the new DFS accumulator from the old one and the current prime.

        newDfsAccum = dfsAccumFn currDfsAccum p

        -- These functions simplify and clarify the recursive calls to this function.

        visitNextPrimeInsteadOfThis = visitValsRec currProd currDfsAccum ps
        visitThisPrimeAgain         = visitValsRec newProd newDfsAccum prms

-- This is a very generic version of primeFactorWalk. It takes a list of 'things' that don't even
-- have to be numbers. The only requirement is that if the limit function indicates we have reached
-- a limit with one of the things, then we can't proceed with any later thing on the
-- list. Numerically, in the prime factor case, that would indicate that the list is sorted and if
-- multiplying by one prime in the list exceeds the limit, so would all the later ones.
-- It takes an accumulator state and a dfs state, a dfs function that updates the dfs state based on
-- a thing on the list and the prior dfs state. The accumulator function takes the old accumulator,
-- the dfs state, and the current thing and returns a new accumulator. The limit function returns
-- true if over a limit given the dfs state.

listDFSWalk :: (b -> Bool) -> a -> b -> (a -> b -> c -> a) -> (b -> c -> b) -> [c] -> a
listDFSWalk limitFn accum dfsAccum accFn dfsAccFn objects = walkRec accum dfsAccum objects
  where
    walkRec acc _ [] = acc
    walkRec acc dfsAcc objss@(obj : objs)
      | limitFn newDfsAcc = acc
      | otherwise = let accNext = newAcc `seq` walkRec newAcc dfsAcc objs
                    in  accNext `seq` walkRec accNext newDfsAcc objss
      where
        newAcc = newDfsAcc `seq` accFn acc newDfsAcc obj
        newDfsAcc = dfsAccFn dfsAcc obj

-- This is similar to the one above, but I removed the current object parameter to the accFn, and
-- I have it call accFn on the initial state of the accumulator and dfsAccumulator. This also
-- uses a nice fold to handle the recursion.
-- Note that it calls the accumulator on the initial state! Different behavior than listDFSWalk

listDFSWalk2 :: (b -> Bool) -> a -> b -> (a -> b -> a) -> (b -> c -> b) -> [c] -> a
listDFSWalk2 limitFn accum dfsAccum accFn dfsAccFn objects
  | limitFn dfsAccum || null objects = accum
  | otherwise = walkRec accum dfsAccum objects
  where
    walkRec acc dfsAcc objs = newAcc `seq` foldl' callRecWalk newAcc dfsObjPairs
      where
        newAcc = accFn acc dfsAcc
        callRecWalk a (dA, obs) = walkRec a dA obs
        dfsObjPairs = takeWhile (not . limitFn . fst) $ genDfsObjPairs objs
        genDfsObjPairs [] = []
        genDfsObjPairs obs = let newDfsAcc = dfsAccFn dfsAcc (head obs)
                             in newDfsAcc `seq` (newDfsAcc, obs) : genDfsObjPairs (tail obs)
