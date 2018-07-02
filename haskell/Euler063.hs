module Euler063 where

nthPowers :: Integral a => a -> [a]
nthPowers n = map (^n) [1..]

nthPowersEqualToDigitCount :: Integral a => a -> [a]
nthPowersEqualToDigitCount n = filter (hasMinDigits n) . takeWhile (hasMaxDigits n) . nthPowers $ n
  where
    hasMaxDigits count x = x < 10 ^ count
    hasMinDigits count x = x >= 10 ^ (count - 1)

nDigitNumbersThatAreAnNthPower :: Integral a => [[a]]
nDigitNumbersThatAreAnNthPower = takeWhile (not . null) . map nthPowersEqualToDigitCount $ [1..]

main :: IO ()
main = print . sum . map length $ nDigitNumbersThatAreAnNthPower

