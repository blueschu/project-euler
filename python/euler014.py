"""
A solution to Project Euler Problem 14.

https://projecteuler.net/problem=14
"""

from typing import Generator, List


def collatz(start: int) -> Generator[int, None, None]:
    """
    Yields a collatz sequence beginning with with start.

    >>> list(collatz(2))
    [2, 1]
    >>> list(collatz(13))
    [13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
    >>> list(collatz(0))
    Traceback (most recent call last):
    ...
    ValueError: Collatz sequence is not defined for non-positive seeds
    """
    if start < 1:
        raise ValueError('Collatz sequence is not defined for non-positive seeds')

    while True:
        yield start

        if start == 1:
            break

        if start % 2 == 0:
            start //= 2
        else:
            start = 3 * start + 1


def longest_collatz(max_seed: int) -> List[int]:
    """
    Returns the longest collatz sequence whose seed is less than max_seed.

    >>> longest_collatz(12)
    [9, 28, 14, 7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
    """
    return max((list(collatz(i)) for i in range(1, max_seed)), key=len)


if __name__ == '__main__':
    TARGET = 1000001
    print(longest_collatz(TARGET)[0])

