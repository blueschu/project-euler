module Euler016 where

import Common (digitSum)

target :: Integer
target = 2 ^ 1000

main :: IO ()
main = print $ digitSum target
