#!/usr/bin/env python3

from typing import List

INPUT = 600851475143

def factorize(num: int) -> List[int]:
    """ Returns a list of the prime factors of num """
    factors = []

    i = 2
    while  num >= i:
        if num % i == 0:
            factors.append(i)
            num /= i
        else:
            # only increment if factor was not found
            i += 1

    return factors

if __name__ == '__main__':
    assert factorize(5) == [5]
    assert factorize(12) == [2, 2, 3]
    assert factorize(36) == [2, 2, 3, 3]
    assert factorize(13*17) == [13, 17]
    print(max(factorize(INPUT)))
