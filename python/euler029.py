"""
A solution to Project Euler Problem 29.

https://projecteuler.net/problem=29
"""

import itertools
from typing import Set


def distinct_powers(cap: int) -> Set[int]:
    """
    Returns all combinations of `a**b` for every value of `a` and `b` in [2, `cap`].

    >>> sorted(distinct_powers(5))
    [4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125]
    """
    return {a ** b for a, b in itertools.product(range(2, cap + 1), repeat=2)}


if __name__ == '__main__':
    target = 100
    print(len(distinct_powers(target)))
