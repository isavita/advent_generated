
import heapq
from collections import defaultdict
from typing import List, Tuple

def main():
    grid = defaultdict(str)
    start = end = None
    as_list = []
    with open("input.txt", "r") as file:
        y = 0
        for line in file:
            for x, b in enumerate(line.strip()):
                p = (x, y)
                grid[p] = b
                if b == 'S':
                    start = p
                elif b == 'E':
                    end = p
                elif b == 'a':
                    as_list.append(p)
            y += 1
    grid[start], grid[end] = 'a', 'z'

    dists = djikstra(grid, end)

    l = dists[start]

    for a in as_list:
        if d := dists.get(a):
            l = min(l, d)
    print(l)

def djikstra(grid: dict, end: Tuple[int, int]) -> dict:
    pq = [(0, end)]
    dist = {end: 0}
    heapq.heapify(pq)
    while pq:
        curr_dist, curr = heapq.heappop(pq)
        for n in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            next_p = (curr[0] + n[0], curr[1] + n[1])
            if next_p not in grid:
                continue
            if ord(grid[curr]) - ord(grid[next_p]) > 1:
                continue
            next_dist = dist[curr] + 1
            if next_p not in dist or next_dist < dist[next_p]:
                dist[next_p] = next_dist
                heapq.heappush(pq, (next_dist, next_p))
    return dist

if __name__ == "__main__":
    main()
