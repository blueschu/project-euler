"""
A solution to Project Euler Problem 7.

https://projecteuler.net/problem=7
"""

import itertools

from common import primes


def nth_prime(n: int) -> int:
    """
    Returns the nth prime integer.

    Examples:
    >>> nth_prime(1)
    2
    >>> nth_prime(6)
    13
    """
    return next(itertools.islice(primes(),  n - 1, None))

if __name__ == '__main__':
    print(nth_prime(10001))

