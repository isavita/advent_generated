
import os
from typing import List, Tuple

class DSU:
    __slots__ = ("parent", "rank")
    def __init__(self, n: int):
        self.parent = list(range(n))
        self.rank = [0] * n

    def find(self, x: int) -> int:
        while self.parent[x] != x:
            self.parent[x] = self.parent[self.parent[x]]
            x = self.parent[x]
        return x

    def union(self, a: int, b: int) -> bool:
        ra, rb = self.find(a), self.find(b)
        if ra == rb:
            return False
        if self.rank[ra] < self.rank[rb]:
            self.parent[ra] = rb
        elif self.rank[ra] > self.rank[rb]:
            self.parent[rb] = ra
        else:
            self.parent[rb] = ra
            self.rank[ra] += 1
        return True


def dist_sq(p1: Tuple[int, int, int], p2: Tuple[int, int, int]) -> int:
    dx = p1[0] - p2[0]
    dy = p1[1] - p2[1]
    dz = p1[2] - p2[2]
    return dx * dx + dy * dy + dz * dz


def main() -> None:
    if not os.path.isfile("input.txt"):
        return

    points: List[Tuple[int, int, int]] = []
    with open("input.txt", "r") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            parts = line.split(",")
            if len(parts) != 3:
                continue
            x, y, z = map(int, parts)
            points.append((x, y, z))

    n = len(points)
    if n < 2:
        return

    # build all edges (distance squared, u, v)
    edges = [
        (dist_sq(points[i], points[j]), i, j)
        for i in range(n) for j in range(i + 1, n)
    ]
    edges.sort(key=lambda e: e[0])

    dsu = DSU(n)
    components = n
    for d, u, v in edges:
        if dsu.union(u, v):
            components -= 1
            if components == 1:
                p1, p2 = points[u], points[v]
                print(f"Connected {p1[0]},{p1[1]},{p1[2]} and {p2[0]},{p2[1]},{p2[2]}")
                print(f"Product of X coordinates: {p1[0] * p2[0]}")
                break


if __name__ == "__main__":
    main()
