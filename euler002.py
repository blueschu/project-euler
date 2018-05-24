"""
A solution to Project Euler Problem 2.

https://projecteuler.net/problem=2
"""

import itertools
from typing import Generator

def fib(zero=False) -> Generator[int, None, None]:
    """
    Yields the elements of the fibonacci sequence, optionally begining with zero.

    Examples:
    >>> from itertools import islice
    >>> next(fib(zero=True))
    0
    >>> next(fib(zero=False))
    1
    >>> list(islice(fib(), 10))
    [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    """
    a = 0 if zero else 1
    b = 1
    while True:
        yield a
        a, b = b, b + a

def solution(cap: int) -> int:
    """ 
    Returns the sum of all even fibonacci numbers not exceeding cap.
    
    Examples:
    >>> solution(10)
    10
    """
    return sum(i for i in itertools.takewhile(lambda x: x < cap, fib()) if i % 2 == 0)

if __name__ == '__main__':
    print(solution(4000000))

