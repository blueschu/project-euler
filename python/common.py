"""
Common utilities for Project Euler Problems
"""

import itertools
import math
from typing import Generator, List, Sequence, Set, Tuple


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

    top = int(math.ceil(math.sqrt(num))) + 1
    return num % 2 != 0 and all(num % i != 0 for i in range(3, top, 2))


def primes() -> Generator[int, None, None]:
    """
    Yields every prime integer.


    Example:
    >>> list(itertools.islice(primes(), 6))
    [2, 3, 5, 7, 11, 13]
    """
    for i in itertools.count(2):
        if is_prime(i):
            yield i


def digit_sum(digits: Sequence[str]) -> int:
    """
    Returns the sum of the digits in the specified string of digits.

    Examples:
        >>> digit_sum('12')
        3
        >>> digit_sum(str(456))
        15
        >>> digit_sum(['6', '7', '5'])
        18
        >>> digit_sum('123abc')
        Traceback (most recent call last):
        ...
        ValueError: invalid literal for int() with base 10: 'a'
    """
    return sum(map(int, digits))


def factor_pairs(num: int) -> List[Tuple[int, int]]:
    """
    Returns a list of all of the factor pairs of num.

    Examples:
        >>> factor_pairs(1)
        [(1, 1)]
        >>> factor_pairs(2)
        [(1, 2)]
        >>> factor_pairs(4)
        [(1, 4), (2, 2)]
        >>> factor_pairs(12)
        [(1, 12), (2, 6), (3, 4)]
        >>> factor_pairs(64)
        [(1, 64), (2, 32), (4, 16), (8, 8)]
        >>> factor_pairs(0)
        []
    """
    factors = []
    for i in range(1, int(num ** 0.5) + 1):
        div, mod = divmod(num, i)
        if mod == 0:
            factors.append((i, div))

    return factors


def factor_set(num: int) -> Set[int]:
    """
    Returns the set of all of the factors of num.
    
    Examples:
        >>> sorted(factor_set(220))
        [1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110, 220]
        >>> factor_set(1)
        {1}
        >>> sorted(factor_set(9))
        [1, 3, 9]
    """
    return set(itertools.chain.from_iterable(factor_pairs(num)))

