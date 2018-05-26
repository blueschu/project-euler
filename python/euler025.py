"""
A solution to Project Euler Problem 24.

https://projecteuler.net/problem=24
"""

import itertools
from typing import Any, Callable, Generator, Iterable

from common import fibonacci


def indexes_matching(predicate: Callable[[Any], bool], it: Iterable[Any]) -> Generator[int, None, None]:
    """
    Yields the indexes in the specified iterable that satisfy the predicate.

    >>> indexes_matching(lambda: True, []) #doctest: +ELLIPSIS
    <generator object indexes_matching at 0x...>
    >>> list(indexes_matching(str.isdigit, 'abc123'))
    [3, 4, 5]
    """
    for i, elem in enumerate(it):
        if predicate(elem):
            yield i


def num_digits(num: int) -> int:
    """
    Returns the number of digits in the decimal representation of num.

    >>> num_digits(0)
    1
    >>> num_digits(9)
    1
    >>> num_digits(121)
    3
    >>> num_digits(10**5)
    6
    >>> num_digits(-2)
    2
    """
    return len(str(num))


if __name__ == '__main__':
    target = int(1e3)
    print(next(indexes_matching(lambda i: num_digits(i) == target, fibonacci(zero=True))))
