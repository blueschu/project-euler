"""
A solution to Project Euler Problem 5.

https://projecteuler.net/problem=5
"""

import itertools


def first_divisible_by_all_below(top_inclusive: int) -> int:
    """
    Returns the first number that is divisible by all numbers in [1, top_inclusive].

    A Brute Force approach.

    Examples:
    >>> first_divisible_by_all_below(4)
    12
    >>> first_divisible_by_all_below(10) # Problem example
    2520
    """
    for i in itertools.count(top_inclusive * (top_inclusive - 1)):
        if all(i % j == 0 for j in range(2, top_inclusive + 1)):
            return i


if __name__ == '__main__':
    print(first_divisible_by_all_below(20))  # produces 232792560
