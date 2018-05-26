"""
A solution to Project Euler Problem 3.

https://projecteuler.net/problem=3
"""

from typing import List

# The number to be factored from the problem's description.
INPUT = 600851475143


def factorize(num: int) -> List[int]:
    """
    Returns a list of the prime factors of num.

    >>> factorize(5)
    [5]
    >>> factorize(12)
    [2, 2, 3]
    >>> factorize(36)
    [2, 2, 3, 3]
    >>> factorize(13 * 17)
    [13, 17]
    """
    factors = []

    i = 2
    while num >= i:
        if num % i == 0:
            factors.append(i)
            num /= i
        else:
            # only increment if factor was not found
            i += 1

    return factors


if __name__ == '__main__':
    print(max(factorize(INPUT)))
