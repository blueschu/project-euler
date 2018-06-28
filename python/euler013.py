"""
A solution to Project Euler Problem 13.

https://projecteuler.net/problem=13
"""

from common import digit_sum


if __name__ == '__main__':
    with open("../resources/input013.txt", "r") as fin:
        numbers = map(int, fin.read().splitlines())

    total = sum(numbers)
    print(str(total)[:10])
