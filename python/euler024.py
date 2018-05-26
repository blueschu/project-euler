"""
A solution to Project Euler Problem 24.

https://projecteuler.net/problem=24
"""

import itertools

OPTIONS = list(range(10))

if __name__ == '__main__':
    target = int(1e6) - 1
    print(next(itertools.islice(itertools.permutations(OPTIONS), target, None)))
