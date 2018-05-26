"""
A solution to Project Euler Problem 4.

https://projecteuler.net/problem=4
"""

import itertools
from typing import Generator, Sequence


def is_palindrome(seq: Sequence) -> bool:
    """
    Returns True is the specified string is a palindrome.

    >>> is_palindrome('12344321')
    True
    >>> is_palindrome('12321')
    True
    >>> is_palindrome('1111111111')
    True
    >>> is_palindrome('abcd')
    False
    """
    # Manually indexing in a loop or comprehension is generally a red
    # flag. However, in this case, I believe that indexing is simplest
    # solution as zipping and reversing slices would quickly become
    # convoluted.
    return all(seq[i] == seq[-i - 1] for i in range(len(seq) // 2))


def products_in_range(range_: range) -> Generator[int, None, None]:
    """
    Yields i*j for every combination of i and j (with replacement) in range_.

    >>> list(products_in_range(range(1, 6)))
    [1, 2, 3, 4, 5, 4, 6, 8, 10, 9, 12, 15, 16, 20, 25]
    """
    for i, j in itertools.combinations_with_replacement(range_, 2):
        yield i * j


def solution(range_: range):
    """
    >>> solution(range(10, 100))
    9009
    """
    return max(i for i in products_in_range(range_) if is_palindrome(str(i)))


if __name__ == '__main__':
    print(solution(range(100, 1000)))
