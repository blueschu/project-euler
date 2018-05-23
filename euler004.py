#!/usr/bin/env python3

from typing import Generator, Sequence

def is_palindrome(seq: Sequence) -> bool:
    """ 
    Returns True is the specified string is a palindrome.
    
    Examples:
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
    # flag. However, in this case, I believe that indexing is simplerly 
    # than zipping and reversing slices.
    return all(seq[i] == seq[-i-1] for i in range(len(seq) // 2)) 

def products_in_range(range_: range) -> Generator[int, None, None]:
    for i in range_:
        for j in range(i, range_.stop):
            yield i * j

def solution(range_: range):
    """
    >>> solution(range(10, 100))
    9009
    """
    return max(i for i in products_in_range(range_) if is_palindrome(str(i)))

if __name__ == '__main__':
    print(solution(range(100, 1000)))

