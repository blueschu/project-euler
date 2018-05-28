"""
A solution to Project Euler Problem 35.

https://projecteuler.net/problem=35
"""

import itertools
import math
from typing import Generator

# Applying functools.lru_cache to is_prime only reduces total execution time by ~0.1 sec
from common import is_prime


def numeric_rotations(num: int) -> Generator[int, None, None]:
    """
    Yields every integer that is a rotation of the decimal digits of `num`, not including `num`.

    >>> list(numeric_rotations(123))
    [312, 231]
    >>> list(numeric_rotations(1234))
    [4123, 3412, 2341]
    >>> list(numeric_rotations(11))
    [11]
    >>> list(numeric_rotations(1))
    []
    """
    top = int(math.log10(num))
    scale = 10 ** top
    for _ in itertools.repeat(None, top):
        div, mod = divmod(num, 10)
        num = div + mod * scale
        yield num


def is_circular_prime(num: int) -> bool:
    """
    Returns True if all rotations of the decimal digits of `num` are prime.

    >>> is_circular_prime(197)
    True
    >>> is_circular_prime(31)
    True
    >>> is_circular_prime(23)
    False
    """
    return is_prime(num) and all(is_prime(p) for p in numeric_rotations(num))


def circular_primes() -> Generator[int, None, None]:
    """
    Yields all of the circular primes.

    >>> list(itertools.islice(circular_primes(), 13))
    [2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, 97]
    """
    for i in itertools.count(2):
        if is_circular_prime(i):
            yield i


def circular_primes_below(cap: int) -> int:
    """
    Returns the number of circular primes that are less than `cap`.

    This implementation can determine circular prime counts for all caps
    less than `999332` in approximately four seconds or less. However,
    beginning at `cap = 999332`, this functions fails to return for
    astonishing long periods of time. At `cap = 1000000`, this functions
    fails to return after over thirty minutes of execution. Thus far, I
    have been unable to determine the cause of this behavior.

    >>> circular_primes_below(100)
    13
    """
    return sum(1 for _ in itertools.takewhile(lambda x: x < cap, circular_primes()))


if __name__ == '__main__':
    target = int(1e6)
    print(circular_primes_below(target))
