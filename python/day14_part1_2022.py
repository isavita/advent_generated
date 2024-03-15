from collections import defaultdict
from dataclasses import dataclass
from typing import Tuple

@dataclass(frozen=True)
class Point:
    x: int
    y: int

    def add(self, other):
        return Point(self.x + other.x, self.y + other.y)

def min(a, b):
    return a if a < b else b

def max(a, b):
    return a if a > b else b

def bounds(grid):
    min_x, min_y = float('inf'), float('inf')
    max_x, max_y = float('-inf'), float('-inf')
    for p in grid:
        min_x, min_y = min(min_x, p.x), min(min_y, p.y)
        max_x, max_y = max(max_x, p.x), max(max_y, p.y)
    return Point(min_x, min_y), Point(max_x + 1, max_y + 1)

def fill(grid):
    floor, sands, first_floor_touch = bounds(grid)[1].y, 0, 0
    while True:
        sand = Point(500, 0)
        while True:
            if sand.y == floor - 1:
                if not first_floor_touch:
                    first_floor_touch = sands
                grid[sand] = True
                break
            for d in [Point(0, 1), Point(-1, 1), Point(1, 1)]:
                new_sand = sand.add(d)
                if new_sand not in grid:
                    sand = new_sand
                    break
            else:
                grid[sand] = True
                break
        sands += 1
        if sand.y == 0:
            return first_floor_touch

def main():
    grid = defaultdict(bool)
    with open("input.txt", "r") as f:
        for line in f:
            points = [Point(*(int(x) for x in p.split(","))) for p in line.strip().split(" -> ")]
            for i in range(len(points) - 1):
                p1, p2 = points[i], points[i + 1]
                if p1.x == p2.x:
                    for y in range(min(p1.y, p2.y), max(p1.y, p2.y) + 1):
                        grid[Point(p1.x, y)] = True
                else:
                    for x in range(min(p1.x, p2.x), max(p1.x, p2.x) + 1):
                        grid[Point(x, p1.y)] = True
    print(fill(grid))

if __name__ == "__main__":
    main()