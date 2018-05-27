"""
A solution to Project Euler Problem 28.

https://projecteuler.net/problem=28
"""


def spiral_diagonal_sum(radius: int) -> int:
    """
    Returns the sum of the diagonal elements on a number spiral with the given radius.

    In a spiral, the top right corner of any ring is a perfect square whose
    value is given by `(2r-1)^2` where `r` is the radius (or index) of the ring.
    The value of the top left corner is equal to the value of the top right
    corner minus `2r-1`. It follows that the bottom left corner is then equal
    to the value of the top right minus `2(2r-1)` and the bottom right is equal
    to the top right minus `3(2r-1)`. The sum of the corners of a particular
    ring may therefore be expressed as `4(top_right) - 6(2r-1)`.

    More concisely, the sum of the corners elements of any ring (box) in a
    spiral whose radius is greater than 1 may be given by the equation
    `16r^2 - 28r + 16`, where `r` is the radius of the ring. The sum of the
    diagonals of the spiral is therefore equal to the sum from `r` equals `2`
    to `r` equals `radius` of `16r^2 - 28r + 16` plus 1.

    >>> spiral_diagonal_sum(1)
    1
    >>> spiral_diagonal_sum(3)
    101
    >>> spiral_diagonal_sum(-2)
    Traceback (most recent call last):
        ...
    ValueError: Spiral radius MUST NOT be less than 1.
    """
    if radius < 1:
        raise ValueError("Spiral radius MUST NOT be less than 1.")
    return sum(16 * ring * ring - 28 * ring + 16 for ring in range(2, radius + 1)) + 1
    # Or, more readable:
    # total = 0
    # for ring in range(2, radius + 1):
    #     corner_square = (2 * ring - 1) ** 2
    #     other_corner_total_different = 6 * (2 * ring - 2)
    #     ring_total = 4 * corner_square - other_corner_total_different
    #     total += ring_total
    # return total + 1


if __name__ == '__main__':
    width = 1001
    spiral_radius = (width + 1) // 2
    print(spiral_diagonal_sum(spiral_radius))
