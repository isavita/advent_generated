
import heapq
from collections import defaultdict
from typing import List, Tuple

def djikstra(grid: dict, end: Tuple[int, int]) -> dict:
    pq = [(0, end)]
    dist = {end: 0}
    while pq:
        d, curr = heapq.heappop(pq)
        for n in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            next = (curr[0] + n[0], curr[1] + n[1])
            if next not in grid:
                continue
            if ord(grid[curr]) - ord(grid[next]) > 1:
                continue
            nextdist = dist[curr] + 1
            if next not in dist or nextdist < dist[next]:
                dist[next] = nextdist
                heapq.heappush(pq, (nextdist, next))
    return dist

def main():
    grid = {}
    with open("input.txt", "r") as file:
        s = file.readlines()
    start, end = None, None
    as_points = []
    y = 0
    for line in s:
        line = line.strip()
        for x, b in enumerate(line):
            p = (x, y)
            grid[p] = b
            if b == 'S':
                start = p
            elif b == 'E':
                end = p
            elif b == 'a':
                as_points.append(p)
        y += 1
    grid[start], grid[end] = 'a', 'z'
    dists = djikstra(grid, end)
    l = dists[start]
    print(l)

if __name__ == "__main__":
    main()
