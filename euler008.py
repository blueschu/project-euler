"""
A solution to Project Euler Problem 8.

https://projecteuler.net/problem=8
"""

import functools
import itertools
import operator
from typing import Generator

# 1000-digit number given in the problem's description.
INPUT = """
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450
""".replace('\n', '')


# From itertools examples
# https://docs.python.org/2.3/lib/itertools-example.html
def window(seq, n=2):
    """Returns a sliding window (of width n) over data from the iterable
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

    Example:
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

    Example:
    >>> solution(INPUT, 4)
    5832
    """
    return max(digit_window_products(digits, n))


if __name__ == '__main__':
    print(solution(INPUT, 13))
