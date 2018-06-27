-- | Common utilities for Project Euler solutions.
module Common
( factorial
, digitSum
, fibonacci
, factorPairs
, factorSet
, numericPalindrome
, digitListBase
, takeLast
, digitFactorialSum
, nPr
, nCr
) where

import Data.List (unfoldr)
import qualified Data.Set as Set
import Data.Tuple (swap)

-- | Computes the factorial of an integer.
factorial :: Integral a => a -> a
factorial n = product [2..n]

-- | Computes the sum of the decimal digits of an integer.
digitSum :: Integral a => a -> a
digitSum n
  | n < 0  = digitSum (-n)
digitSum n = fst $ until done step (0, n)
  where
    done (_, rest) = rest == 0
    step (acc, rest) =
        let (front, end) = rest `divMod` 10
        in  acc `seq` (acc + end, front)

-- | The elements of the fibonacci sequences, beginning with 0.
fibonacci :: Integral a => [a]
fibonacci = map fst $ iterate (\(a,b) -> (b, b + a)) (0, 1)

-- | Yields an ordered list of the factor pairs of an integer.
factorPairs :: Integral a => a -> [(a,a)]
factorPairs n = map partner . filter isFactor $ [1..top]
  where
    top        = floor . sqrt . fromIntegral $ n
    partner x  = (x, n `div` x)
    isFactor x = n `mod` x == 0

-- | Yields a set of all of the factors of an integer.
factorSet :: Integral a => a -> Set.Set a
factorSet = Set.fromList . flattenPairs . factorPairs where
    flattenPairs = foldr (\(a, b) acc -> a : b : acc) []

-- | True if the given integer is palindromic when represented in the given base.
numericPalindrome :: Integral a 
                  => a    -- ^ The base to use. 
                  -> a    -- ^ The integer to be checked.
                  -> Bool -- ^ Whether or not the integer is palindromic with respect to the given base.
numericPalindrome base n
  | n < 0 = numericPalindrome base (-n)
  | otherwise = let logBaseBase         = logBase (fromIntegral base)
                    greatestPower       = floor . logBaseBase . fromIntegral
                    greatestPowerOfBase = base ^ (greatestPower n)
                in  decayPalindrome n greatestPowerOfBase
  where
    decayPalindrome rest top
      | top == 0 = True
      | otherwise = left == right && decayPalindrome endsDecayed nextTop
      where
        left  = rest `quot` top
        right = rest `mod` base
        endsDecayed = (rest - (left * top) - right) `quot` base
        nextTop = top `quot` (base ^ 2)

-- | Yields a list of the digits in the representation of an integer in the given base.
digitListBase :: Integral a 
              => a   -- ^ The base to use.
              -> a   -- ^ The value whose digits are to be produced.
              -> [a] -- ^ List of the digits used to represent the value in the given base.
digitListBase _ 0 = [0]
digitListBase base n
  | n < 0     = error "Not defined for negative numbers"
  | base < 2  = error "Not defined for bases less than 2"
  | otherwise = reverse $ unfoldr step n
  where
    step 0 = Nothing
    step r = Just . swap $ r `divMod` base

-- | Takes the last n elements of a list.
-- Based on formula given at <https://www.joachim-breitner.de/blog/600-On_taking_the_last_n_elements_of_a_list>.
takeLast :: Int -> [a] -> [a]
takeLast n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys

-- | Computes the sum of the factorials of the decimal digits of an integer.
--
-- Factorial function replaced with local patterning matching to reduce computation time.
digitFactorialSum :: Integral a => a -> a
digitFactorialSum = sum . map digitFactorial . digitListBase 10
  where
    digitFactorial :: Integral a => a -> a
    digitFactorial 0 = 1
    digitFactorial 1 = 1
    digitFactorial 2 = 2
    digitFactorial 3 = 6
    digitFactorial 4 = 24
    digitFactorial 5 = 120
    digitFactorial 6 = 720
    digitFactorial 7 = 5040
    digitFactorial 8 = 40320
    digitFactorial 9 = 362880

-- | The number of permutaitons of n objects taken r at a time.
nPr :: Integral a => a -> a -> a
n `nPr` r = (factorial n) `quot` (factorial (n - r))

-- | The number of combinations of n objects taken r at a time.
nCr :: Integral a => a -> a -> a
n `nCr` r
  | n == r = 1
  | n < r  = 0
  | otherwise = (n `nPr` r) `quot` (factorial r)
