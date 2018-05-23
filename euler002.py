#!/usr/bin/env python3

import itertools
from typing import Generator

def fib(zero=False) -> Generator[int, None, None]:
    a = 0 if zero else 1
    b = 1
    while True:
        yield a
        a, b = b, b + a

def solution(cap: int) -> int:
    """ The sum of all even fibonacci numbers not exceeding cap"""
    return sum(i for i in itertools.takewhile(lambda x: x < cap, fib()) if i % 2 == 0)

if __name__ == '__main__':
    assert solution(10) == 10
    print(solution(4000000))

