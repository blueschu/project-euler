"""
A solution to Project Euler Problem 2.

https://projecteuler.net/problem=2
"""

import itertools

from common import fibonacci


def solution(cap: int) -> int:
    """
    Returns the sum of all even fibonacci numbers not exceeding cap.

    >>> solution(10)
    10
    """
    return sum(i for i in itertools.takewhile(lambda x: x < cap, fibonacci()) if i % 2 == 0)


if __name__ == '__main__':
    print(solution(4000000))
