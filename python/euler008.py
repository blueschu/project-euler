"""
A solution to Project Euler Problem 8.

https://projecteuler.net/problem=8
"""

import functools
import itertools
import operator
from typing import Generator


# From itertools examples
# https://docs.python.org/2.3/lib/itertools-example.html
def window(seq, n=2):
    """
    Returns a sliding window (of width n) over data from the iterable.

    s -> (s0,s1,...s[n-1]), (s1,s2,...,sn), ...
    """
    it = iter(seq)
    result = tuple(itertools.islice(it, n))
    if len(result) == n:
        yield result
    for elem in it:
        result = result[1:] + (elem,)
        yield result


def digit_window_products(digits: str, n: int) -> Generator[int, None, None]:
    """
    Returns the product of the digits in a sliding window of width n
    over the string of digits.

    >>> gen = digit_window_products('1234', 2)
    >>> next(gen)
    2
    >>> next(gen)
    6
    >>> next(gen)
    12
    >>> next(gen)
    Traceback (most recent call last):
    ...
    StopIteration
    """
    for win in window(digits, n):
        yield functools.reduce(operator.mul, map(int, win))


def solution(digits: str, n: int) -> int:
    """
    Returns the greatest product of n adjacent digits in the given
    digits string.

    >>> solution(INPUT, 4)
    5832
    """
    return max(digit_window_products(digits, n))


if __name__ == '__main__':
    with open("../resources/input008.txt", "r") as fin:
        digits = fin.read().replace("\n", "")

    print(solution(digits, 13))
