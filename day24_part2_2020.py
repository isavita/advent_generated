
from collections import defaultdict

directions = {'e': (1, 0), 'se': (0, -1), 'sw': (-1, -1), 'w': (-1, 0), 'nw': (0, 1), 'ne': (1, 1)}

tiles = defaultdict(bool)
for line in open("input.txt").read().splitlines():
    x, y = 0, 0
    i = 0
    while i < len(line):
        if line[i] in directions:
            dx, dy = directions[line[i]]
            i += 1
        else:
            dx, dy = directions[line[i:i+2]]
            i += 2
        x, y = x + dx, y + dy
    tiles[(x, y)] = not tiles[(x, y)]

def get_adjacent(x, y):
    return [(x+dx, y+dy) for dx, dy in directions.values()]

def count_adjacent(tiles, x, y):
    return sum(tiles[(nx, ny)] for nx, ny in get_adjacent(x, y))

for _ in range(100):
    new_tiles = defaultdict(bool)
    for x in range(-100, 100):
        for y in range(-100, 100):
            adj_count = count_adjacent(tiles, x, y)
            if tiles[(x, y)]:
                new_tiles[(x, y)] = adj_count in [1, 2]
            else:
                new_tiles[(x, y)] = adj_count == 2
    tiles = new_tiles

print(sum(tiles.values()))
