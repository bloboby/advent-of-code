import copy
import itertools
import sys
from dataclasses import dataclass
from typing import Dict, List, Set, Tuple

Label = int
Coord = Tuple[int, int]
Adj = Dict[Label, Set[Label]]


@dataclass
class Brick:
    label: Label
    xys: List[Coord]
    z: int
    height: int

    def __lt__(self, other: "Brick"):
        return self.z < other.z


def parse() -> List[Brick]:
    bricks: List[Brick] = []

    contents = sys.stdin.read()
    for label, line in enumerate(contents.splitlines()):
        [[a, b, c], [d, e, f]] = [
            [int(x) for x in brick.split(",")] for brick in line.split("~")
        ]
        xmin, xmax, ymin, ymax = min(a, d), max(a, d) + 1, min(b, e), max(b, e) + 1
        xys = list(itertools.product(range(xmin, xmax), range(ymin, ymax)))
        height = abs(f - c) + 1
        bricks.append(Brick(label, xys, min(c, f), height))

    return bricks


def graph(bricks: List[Brick]) -> Tuple[Adj, Adj]:
    highest: Dict[Coord, Tuple[int, Label]] = {}
    needs: Adj = {}
    supports: Adj = {brick.label: set() for brick in bricks}

    for brick in bricks:
        # Get its landing z-coord and all supporting labels.
        zmax = 0
        labels: Set[Label] = set()
        for xy in brick.xys:
            z, label = highest.get(xy, (0, None))
            if z > zmax:
                zmax = z
                labels = set()
            if z == zmax and label is not None:
                labels.add(label)

        # Update the state.
        for xy in brick.xys:
            highest[xy] = (zmax + brick.height, brick.label)

        needs[brick.label] = labels
        for label in labels:
            supports[label].add(brick.label)

    return (needs, supports)


def disintegrate(needs: Adj, supports: Adj, curr: Label) -> Set[int]:
    ans: Set[Label] = {curr}

    for s in supports[curr]:
        needs[s].remove(curr)
        if not needs[s]:
            ans = ans.union(disintegrate(needs, supports, s))

    return ans


def main() -> None:
    bricks = parse()
    bricks.sort()
    (needs, supports) = graph(bricks)

    # Part 1
    needed: Set[Label] = set()
    for labels in needs.values():
        if len(labels) == 1:
            needed = needed.union(labels)

    print(len(bricks) - len(needed))

    # Part 2
    fallen: int = 0
    for brick in bricks:
        labels = disintegrate(
            copy.deepcopy(needs), copy.deepcopy(supports), brick.label
        )
        fallen += len(labels) - 1

    print(fallen)


main()
