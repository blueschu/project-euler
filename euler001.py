"""
A solution to Project Euler Problem 1.

https://projecteuler.net/problem=1
"""


def solution(cap: int) -> int:
    """
    Returns the sum of all multiples of 3 or five less than cap

    Examples:
    >>> solution(10) # Problem example
    23
    """
    return sum(i for i in range(cap) if i % 3 == 0 or i % 5 == 0)


if __name__ == '__main__':
    print(solution(1000))
