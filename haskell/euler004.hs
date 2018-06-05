numericPalindrome :: Integral a => a -> a -> Bool
numericPalindrome base n
    | n < 0 = numericPalindrome base (-n)
    | otherwise = decayPalindrome base n greatestPowerOfBase
    where greatestPowerOfBase = base ^ (floor . logBase (fromIntegral base) . fromIntegral) n
          decayPalindrome base n top
              | n < base = True
              | otherwise = left == right && decayPalindrome base endsDecayed nextTop
              where left = n `quot` top
                    right = n `mod` base
                    endsDecayed = (n - (left * top) - right) `quot` base
                    nextTop = top `quot` (base ^ 2)

greatestPalindrome :: Integral a => [a] -> a
greatestPalindrome = (maximum . filter (numericPalindrome 10))

productsOfNDigitNumbers :: Integral a => a -> [a]
productsOfNDigitNumbers digits = [a * b | a <- [bottom..top], b <- [a..top]]
    where bottom = 10 ^ (digits - 1)
          top = 10 ^ digits - 1

main = print $ greatestPalindrome $ productsOfNDigitNumbers 3
