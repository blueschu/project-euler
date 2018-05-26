"""
A solution to Project Euler Problem 12.

https://projecteuler.net/problem=12
"""

import itertools
from typing import Generator

from common import factor_pairs


def triangle_numbers() -> Generator[int, None, None]:
    """
    Yields every triangle number.

    >>> list(itertools.islice(triangle_numbers(), 10))
    [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
    """
    num = 0
    for i in itertools.count(1):
        num += i
        yield num


def solution(num_divisors: int) -> int:
    """
    Returns the first triangle number that has at least num_divisors factors.

    >>> solution(5)
    28
    """
    for i in triangle_numbers():
        if len(factor_pairs(i)) * 2 >= num_divisors:
            return i


if __name__ == '__main__':
    print(solution(500))
