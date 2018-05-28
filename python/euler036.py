"""
A solution to Project Euler Problem 36.

https://projecteuler.net/problem=36
"""

import itertools
import math
from typing import Generator


def is_numeric_palindrome(num: int, base: int = 10) -> bool:
    """
    Returns True if the representation of `num` in the given `base` is a palindrome.

    >>> is_numeric_palindrome(1001, base=10)
    True
    >>> is_numeric_palindrome(1011, base=10)
    False
    >>> is_numeric_palindrome(1234554321, base=10)
    True
    >>> is_numeric_palindrome(123454321, base=10)
    True
    >>> is_numeric_palindrome(585, base=2)
    True
    >>> is_numeric_palindrome(587, base=2)
    False
    """
    top = int(math.log(num, base))
    for exp in range(int(math.ceil(top / 2))):
        left = int(num / (base ** exp)) % base
        right = int(num / (base ** (top - exp))) % base
        if left != right:
            return False
    return True


def palindromic_numbers(*bases: int) -> Generator[int, None, None]:
    """
    Yields all numbers whose representation in all of the given `bases` is a palindrome.

    >>> list(itertools.islice(palindromic_numbers(10), 21))
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 22, 33, 44, 55, 66, 77, 88, 99, 101, 111, 121]
    """
    for i in itertools.count(1):
        if all(is_numeric_palindrome(i, b) for b in bases):
            yield i


if __name__ == '__main__':
    target = int(1e6)
    print(sum(itertools.takewhile(lambda x: x < target, palindromic_numbers(10, 2))))
