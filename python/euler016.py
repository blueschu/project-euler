"""
A solution to Project Euler Problem 16.

https://projecteuler.net/problem=16
"""

from common import digit_sum


def power_of_two_digit_sum(power: int) -> int:
    """
    Returns the sum of the decimal digits of 2^power.

    >>> power_of_two_digit_sum(15)  # Problem example
    26
    """
    return digit_sum(str(2**power))


if __name__ == '__main__':
    power = 1000
    print(power_of_two_digit_sum(power))

