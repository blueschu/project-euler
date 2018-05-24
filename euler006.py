"""
A solution to Project Euler Problem 6.

https://projecteuler.net/problem=6
"""

def sum_of_squares_below(top: int) -> int:
    """ 
    Returns the sum of the squares of all positive intergers less than or
    equal to top.
    
    Example:
    >>> sum_of_squares_below(10) # Problem example
    385
    """
    return sum(i*i for i in range(1, top + 1))

def square_of_sum_below(top: int) -> int:
    """
    Returns the square of the sum of all positive integers less than or
    equal to top.

    Example:
    >>> square_of_sum_below(10) # Problem example
    3025
    """
    return int((1 + top) * (top / 2)) ** 2

def solution(num: int) -> int:
    """
    Returns the difference between the square of the sum of the first
    num natural numbers and the sum of the squares of the first num 
    natural numbers.

    Example:
    >>> solution(10) # Problem example
    2640
    """
    return square_of_sum_below(num) - sum_of_squares_below(num)

if __name__ == '__main__':
    print(solution(100))
