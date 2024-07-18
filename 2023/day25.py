import copy
import sys
from collections import defaultdict, deque
from typing import Deque, Dict, List, Set, Tuple

Edges = Set[Tuple[str, str]]


def min_path(adj, used_edges: Edges, start: str, end: str) -> Edges:
    bfs: Deque[Tuple[str, List[str]]] = deque([(start, [])])
    vis: Set[str] = set()

    while bfs:
        node, path = bfs.popleft()
        path.append(node)

        if node in vis:
            continue
        vis.add(node)

        if node == end:
            min_path = set(zip(path, path[1:]))
            min_path |= {(y, x) for (x, y) in min_path}
            return min_path

        for nbr in adj[node]:
            if (node, nbr) in used_edges:
                continue
            bfs.append((nbr, copy.deepcopy(path)))

    return set()


def component(adj, used_edges: Edges, root: str) -> Set[str]:
    bfs: Deque[str] = deque([root])
    vis: Set[str] = set()

    while bfs:
        node = bfs.popleft()
        if node in vis:
            continue
        vis.add(node)
        bfs.extend([nbr for nbr in adj[node] if (node, nbr) not in used_edges])

    return vis


def main() -> None:
    adj: Dict[str, Set[str]] = defaultdict(set)
    contents = sys.stdin.read()
    for line in contents.splitlines():
        words = line.split()
        x = words[0][:-1]
        for y in words[1:]:
            adj[x].add(y)
            adj[y].add(x)

    root: str = min(adj)
    for node in adj.keys():
        if node == root:
            continue

        used_edges: Edges = set()
        for _ in range(3):
            used_edges |= min_path(adj, used_edges, root, node)

        if min_path(adj, used_edges, root, node):
            continue

        a = component(adj, used_edges, root)
        b = component(adj, used_edges, node)
        print(len(a) * len(b))
        break


main()
