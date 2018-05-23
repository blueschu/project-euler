#!/usr/bin/env python3

import itertools

from euler007 import primes

def sum_of_primes_below(n: int) -> int:
    """
    Returns the sum of all of the prime integers that are less than n

    Example:
    >>> sum_of_primes_below(10)
    17
    """
    return sum(itertools.takewhile(lambda x: x < n, primes()))

if __name__ == '__main__':
    print(sum_of_primes_below(2000000)) # produces 142913828922

