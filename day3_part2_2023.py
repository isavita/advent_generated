
from collections import namedtuple

Neighbor = namedtuple('Neighbor', ['x', 'y'])

neighbors_8 = [Neighbor(0, 1), Neighbor(0, -1), Neighbor(1, 0), Neighbor(-1, 0),
               Neighbor(-1, -1), Neighbor(-1, 1), Neighbor(1, -1), Neighbor(1, 1)]

class Part:
    def __init__(self, xmin, xmax, y, n):
        self.xmin = xmin
        self.xmax = xmax
        self.y = y
        self.n = n

    def valid(self, grid):
        for x in range(self.xmin, self.xmax + 1):
            for neighbor in neighbors_8:
                new_x = x + neighbor.x
                new_y = self.y + neighbor.y
                if (new_x, new_y) in grid:
                    c = grid[(new_x, new_y)]
                    if c != '.' and not ('0' <= c <= '9'):
                        return True
        return False

with open('input.txt', 'r') as file:
    input_data = file.read().strip()

grid = {}
parts = []
curr = None
for y, line in enumerate(input_data.split('\n')):
    if curr:
        parts.append(curr)
        curr = None
    for x, c in enumerate(line):
        grid[(x, y)] = c
        if '0' <= c <= '9':
            if not curr:
                curr = Part(x, x, y, int(c))
            else:
                curr.n = curr.n * 10 + int(c)
                curr.xmax = x
        elif curr:
            parts.append(curr)
            curr = None

parts_grid = {}
for i, p in enumerate(parts):
    for x in range(p.xmin, p.xmax + 1):
        parts_grid[(x, p.y)] = i

sum_val = 0
for p, c in grid.items():
    if c == '*':
        neighbor_parts = set()
        for neighbor in neighbors_8:
            neighbor_pos = (p[0] + neighbor.x, p[1] + neighbor.y)
            if neighbor_pos in parts_grid:
                neighbor_parts.add(parts_grid[neighbor_pos])
        if len(neighbor_parts) == 2:
            prod = 1
            for i in neighbor_parts:
                prod *= parts[i].n
            sum_val += prod

print(sum_val)
