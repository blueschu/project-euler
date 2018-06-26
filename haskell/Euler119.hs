module Euler119 where

import Common (digitListBase)

target :: Int
target = 30

isDigitPower :: Integral a => a -> Bool
isDigitPower n
  | n < 10 || digitSum == 1 = False
  | otherwise = go 2
  where
    digitSum = sum . digitListBase 10 $ n
    go power = case digitPowerSum `compare` n of
        EQ -> True
        GT -> False
        LT -> go (succ power)
      where
        digitPowerSum = digitSum ^ power

main :: IO ()
main = print $ digitPowerSums !! (target - 1) where
    -- Numbers who are equal to their digit sum raised to some power
    digitPowerSums = filter isDigitPower [1..]
