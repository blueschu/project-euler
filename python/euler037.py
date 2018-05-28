"""
A solution to Project Euler Problem 37.

https://projecteuler.net/problem=37
"""

import itertools
import math
from typing import Generator

from common import is_prime


def is_truncatable_prime(num: int) -> bool:
    """
    Returns True is num is a truncatable prime.

    >>> is_truncatable_prime(3797)
    True
    >>> is_truncatable_prime(37)
    True
    >>> is_truncatable_prime(41)
    False
    >>> is_truncatable_prime(77)
    False
    >>> is_truncatable_prime(4)
    False
    >>> is_truncatable_prime(301997)
    False
    """
    if not is_prime(num) or num <= 7:
        return False

    top_power = int(math.log10(num))
    left = right = num
    for _ in itertools.repeat(None, top_power):
        left %= 10 ** top_power
        right //= 10
        if not is_prime(left) or not is_prime(right):
            return False
        top_power -= 1

    return True


def truncatable_primes() -> itertools.islice:
    """
    Yields every truncatable prime.

    >>> list(truncatable_primes())
    [23, 37, 53, 73, 313, 317, 373, 797, 3137, 3797, 739397]

    """

    def _gen() -> Generator[int, None, None]:
        for i in itertools.count(11):
            if is_truncatable_prime(i):
                yield i

    return itertools.islice(_gen(), 11)


if __name__ == '__main__':
    print(sum(truncatable_primes()))
