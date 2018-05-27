"""
A solution to Project Euler Problem 18.

https://projecteuler.net/problem=18
"""

from typing import Sequence

TRIANGLE = (
    (75,),
    (95, 64),
    (17, 47, 82),
    (18, 35, 87, 10),
    (20, 4, 82, 47, 65),
    (19, 1, 23, 75, 3, 34),
    (88, 2, 77, 73, 7, 63, 67),
    (99, 65, 4, 28, 6, 16, 70, 92),
    (41, 41, 26, 56, 83, 40, 80, 70, 33),
    (41, 48, 72, 33, 47, 32, 37, 16, 94, 29),
    (53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14),
    (70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57),
    (91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48),
    (63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31),
    (4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23),
)


def find_greatest_route_sum(triangle: Sequence[Sequence[int]], from_depth=0, from_index=0) -> int:
    """
    Returns the greatest sum that may be produced by descending to adjacent
    elements in the specified triangle.

    It is assumed the the given triangle is well-formed, with each row
    containing exactly one more element than the last.

    Note: this implementation is recursive and is therefore liable to
    overflowing the callstack when used on large triangles.

    >>> example = ((3,),(7, 4,),(2, 4, 6,),(8, 5, 9, 3,),)
    >>> find_greatest_route_sum(example)
    23
    >>> find_greatest_route_sum(example, 2, 2)
    15
    """
    value = triangle[from_depth][from_index]
    if from_depth == len(triangle) - 1:
        return value

    next_depth = from_depth + 1
    return value + max(
        find_greatest_route_sum(triangle, next_depth, from_index),
        find_greatest_route_sum(triangle, next_depth, from_index + 1)
    )


if __name__ == '__main__':
    print(find_greatest_route_sum(TRIANGLE))
