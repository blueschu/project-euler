"""
A solution to Project Euler Problem 21.

https://projecteuler.net/problem=21
"""

import functools
import itertools
from typing import Generator

from common import factor_set


@functools.lru_cache()
def proper_divisor_sum(num: int) -> int:
    """
    Returns the sum of all numbers less than num which divide evenly
    into num.

    Examples:
        >>> proper_divisor_sum(12) # == 1 + 2 + 3 + 4 + 6
        16
        >>> proper_divisor_sum(220)
        284
        >>> proper_divisor_sum(284)
        220
        >>> proper_divisor_sum(1)
        0
        >>> proper_divisor_sum(9)
        4
    """
    return sum(factor_set(num)) - num


def is_amicable(num: int) -> bool:
    """
    Returns True if num is an amicable number.

    Examples:
        >>> is_amicable(284)
        True
        >>> is_amicable(220)
        True
        >>> is_amicable(9)
        False
    """
    divisor_sum = proper_divisor_sum(num)
    return divisor_sum != num and proper_divisor_sum(divisor_sum) == num


def amicable_numbers() -> Generator[int, None, None]:
    """
    Yields every amicable number.

    Examples:
        >>> list(itertools.islice(amicable_numbers(), 2))
        [220, 284]
    """
    for i in itertools.count():
        if is_amicable(i):
            yield i


def amicable_numbers_sum_below(cap: int) -> int:
    """
    Retursn the sum of all of the amicable numbers less than cap.

    Examples:
        >>> amicable_numbers_sum_below(500)
        504
    """
    return sum(itertools.takewhile(lambda x: x < cap, amicable_numbers()))


if __name__ == '__main__':
    target = 10000
    print(amicable_numbers_sum_below(target))
