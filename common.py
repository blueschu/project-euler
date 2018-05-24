"""
Common utilities for Project Euler Problems
"""

import itertools
import math
from typing import Generator


def is_prime(num: int) -> bool:
    """
    Returns True if num is a prime number, else False.

    A simple, brute-force approach.

    Examples:
    >>> all(is_prime(i) for i in (2, 3, 5, 7, 11, 13, 163))
    True
    >>> any(is_prime(i) for i in (0, 1, 9, 10))
    False
    """
    if num == 1:
        return False
    if num == 2:
        return True

    top = math.ceil(math.sqrt(num)) + 1
    return num % 2 != 0 and all(num % i != 0 for i in range(3, top, 2))


def primes() -> Generator[int, None, None]:
    """
    Yields every prime interger.


    Example:
    >>> list(itertools.islice(primes(), 6))
    [2, 3, 5, 7, 11, 13]
    """
    for i in itertools.count(2):
        if is_prime(i):
            yield i
