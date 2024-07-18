import sys
import itertools
from collections import defaultdict
from enum import Enum, auto
from typing import List, Dict, Set
from math import sqrt, ceil

kMin = 200000000000000
kMax = 400000000000000


class Result(Enum):
    OK = auto()
    NO_COLLISION = auto()
    PAST = auto()
    OUT_OF_BOUNDS = auto()


def parse(line: str) -> List[int]:
    [a, b, c, _, d, e, f] = line.replace(",", "").split()
    return [int(x) for x in [a, b, c, d, e, f]]


def coeffs(h: List[int]) -> List[int]:
    [a, b, _, c, d, _] = h
    return [d, -c, b * c - a * d]


def future(h: List[int], x: float, y: float) -> bool:
    [a, b, _, c, d, _] = h
    futureX = (c > 0) == (x > a)
    futureY = (d > 0) == (y > b)

    if futureX != futureY:
        raise RuntimeError(f"Different futures for h:{h} x:{x} y:{y}")

    return futureX


def collision(h1, h2):
    [a, b, c], [d, e, f] = coeffs(h1), coeffs(h2)
    denomX = a * e - b * d
    denomY = b * d - a * e

    if denomX == 0 or denomY == 0:
        return Result.NO_COLLISION, None

    numX = b * f - e * c
    numY = a * f - d * c
    x, y = numX / denomX, numY / denomY

    if not future(h1, x, y) or not future(h2, x, y):
        return Result.PAST, (x, y)

    if x < kMin or x > kMax or y < kMin or y > kMax:
        return Result.OUT_OF_BOUNDS, (x, y)

    return Result.OK, (x, y)


def factorise(n: int) -> List[int]:
    if n == 1:
        return [1]

    factors = set()
    for k in range(1, ceil(sqrt(n))):
        if n % k == 0:
            factors.add(k)
            factors.add(n // k)

    return list(factors)


def velocity_candidates(v: int, dist: int) -> Set[int]:
    candidates = []
    factors = factorise(dist)
    candidates.extend([v - f for f in factors])
    candidates.extend([v + f for f in factors])
    return set(candidates)


def determine_velocity(velocities: Dict[int, List[int]]) -> int:
    restrictions = [
        (v, abs(x1 - x2))
        for v, xs in velocities.items()
        for x1, x2 in itertools.combinations(xs, 2)
    ]

    candidates: Set[int] = velocity_candidates(*restrictions[0])
    for v, dist in restrictions[1:]:
        candidates &= velocity_candidates(v, dist)
        if len(candidates) == 1:
            return min(candidates)

    # Only works on the real input because it turns out to have a unique candidate.
    assert False


def main() -> None:
    contents = sys.stdin.read()
    hailstones = [parse(line) for line in contents.splitlines()]

    part1 = 0
    for h1, h2 in itertools.combinations(hailstones, 2):
        if collision(h1, h2)[0] == Result.OK:
            part1 += 1
    print(part1)

    # Part 2
    vs = []
    for i in range(3):
        velocities: Dict[int, List[int]] = defaultdict(list)
        for h in hailstones:
            velocities[h[i + 3]].append(h[i])
        vs.append(determine_velocity(velocities))

    h0, h1 = hailstones[0], hailstones[1]
    for i in range(3):
        h0[i + 3] -= vs[i]
        h1[i + 3] -= vs[i]

    _, (x, y) = collision(h0, h1)
    h0 = [h0[0], h0[2], None, h0[3], h0[5], None]
    h1 = [h1[0], h1[2], None, h1[3], h1[5], None]
    _, (_, z) = collision(h0, h1)
    print(x + y + z)


main()
