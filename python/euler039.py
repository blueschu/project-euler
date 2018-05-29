"""
A solution to Project Euler Problem 39.

https://projecteuler.net/problem=39
"""

import itertools
import math
import operator
from collections import Counter, namedtuple
from typing import Generator

TriangleListing = namedtuple('TriangleListing', ['sides', 'perimeter'])


def integral_right_triangles(perimeter_cap: int) -> Generator[TriangleListing, None, None]:
    """
    Yields all right triangles with integral sides whose perimeter is less than or equal to `perimeter_cap`.

    >>> list(sorted(t.sides) for t in integral_right_triangles(1000) if t.perimeter == 120)
    [[20, 48, 52.0], [24, 45, 51.0], [30, 40, 50.0]]
    """
    for i in range(3, perimeter_cap - 1):
        for j in itertools.count(i + 1):
            hypot = math.hypot(i, j)
            perimeter = hypot + i + j
            if perimeter > perimeter_cap:
                break
            if hypot.is_integer():
                yield TriangleListing({i, j, hypot}, perimeter)


if __name__ == '__main__':
    target = 1000
    perimeter_counts = Counter(map(operator.attrgetter('perimeter'), integral_right_triangles(1000)))
    most_common_perimeter = perimeter_counts.most_common(1)[0]
    print(most_common_perimeter)
