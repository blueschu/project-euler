"""
A solution to Project Euler Problem 12.

https://projecteuler.net/problem=12
"""

import itertools
from typing import Generator, List, Tuple


def triangle_numbers() -> Generator[int, None, None]:
    """
    Yields every triangle number.

    Examples:
        >>> list(itertools.islice(triangle_numbers(), 10))
        [1, 3, 6, 10, 15, 21, 28, 36, 45, 55]
    """
    num = 0
    for i in itertools.count(1):
        num += i
        yield num


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


def solution(num_divisors: int) -> int:
    """
    Returns the first triangle number that has at least num_divisors factors.

    Examples:
        >>> solution(5)
        28
    """
    for i in triangle_numbers():
        if len(factor_pairs(i)) * 2 >= num_divisors:
            return i


if __name__ == '__main__':
    print(solution(500))
