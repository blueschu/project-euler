"""
A solution to Project Euler Problem 20.

https://projecteuler.net/problem=20
"""

import math
from common import digit_sum


def factorial_digit_sum(num: int) -> int:
    """
    Returns the sum of the decimal digits in num!.
    
    Examples:
        >>> factorial_digit_sum(10)
        27
    """
    return digit_sum(str(math.factorial(num)))


if __name__ == '__main__':
    target = 100
    print(factorial_digit_sum(target))
