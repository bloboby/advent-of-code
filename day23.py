import sys
import copy
from collections import deque
from typing import Dict, Tuple, List, Set

Coord = Tuple[int, int]
Adj = Dict[Coord, Set[Tuple[int, Coord]]]

grid: List[str] = []


def get_nbrs(xy: Coord) -> List[Coord]:
    """
    get_nbrs(orig) = all immediate neighbours, regardless of vis
    get_nbrs(.) = [next], one direction only
    get_nbrs(v) = [next orig], regardless of vis
    """
    n, m = len(grid), len(grid[0])
    x, y = xy

    nbrs = []
    for i, j, c in [(x + 1, y, "v"), (x - 1, y, "^"), (x, y + 1, ">"), (x, y - 1, "<")]:
        if i < 0 or j < 0 or i >= n or j >= m:
            continue
        if grid[x][y] == c and grid[i][j] == ".":
            return [(i, j)]
        if grid[i][j] == "." or grid[i][j] == c:
            nbrs.append((i, j))

    return nbrs


def follow_path(vis: Set[Coord], xy: Coord, dist: int) -> Tuple[int, Coord]:
    vis.add(xy)

    x, y = xy
    if x == len(grid) - 1:
        return (dist, xy)

    nbrs = [nbr for nbr in get_nbrs(xy) if nbr not in vis]
    if grid[x][y] != "." and dist > 1:
        return (dist + 1, nbrs[0])

    return follow_path(vis, nbrs[0], dist + 1)


def get_edges(start: Coord, end: Coord) -> Adj:
    vis = set()
    edges: Adj = {}
    vertices = deque([start])

    while vertices:
        curr = vertices.popleft()
        if curr == end or curr in vis:
            continue
        vis.add(curr)

        dests = [follow_path(set([curr]), nbr, 1) for nbr in get_nbrs(curr)]
        edges[curr] = set(dests)
        vertices.extend([dest for (_, dest) in dests])

    return edges


def longest_path(
    edges: Adj, end: Coord, curr: Coord, dist: int, vis: Set[Coord]
) -> int:
    if curr == end:
        return dist

    max_dist = 0
    for d, nbr in edges[curr]:
        if nbr in vis:
            continue

        new_vis = copy.deepcopy(vis)
        new_vis.add(curr)
        max_dist = max(max_dist, longest_path(edges, end, nbr, dist + d, new_vis))

    return max_dist


def main() -> None:
    global grid
    grid = sys.stdin.read().splitlines()

    start = (0, grid[0].find("."))
    end = (len(grid) - 1, grid[-1].find("."))
    edges = get_edges(start, end)

    part1 = longest_path(edges, end, start, 0, set())
    print(part1)

    # Add undirected edges.
    for xy, nbrs in edges.items():
        for dist, nbr in nbrs:
            if nbr in edges:
                edges[nbr].add((dist, xy))

    part2 = longest_path(edges, end, start, 0, set())
    print(part2)


main()
