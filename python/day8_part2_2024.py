
import math

def gcd(a, b):
    if b == 0:
        return abs(a)
    return gcd(b, a % b)

def solve():
    with open("input.txt") as f:
        grid = f.readlines()

    h = len(grid)
    w = len(grid[0].strip())
    antennas = {}
    for y in range(h):
        for x, c in enumerate(grid[y].strip()):
            if c != '.':
                antennas.setdefault(c, []).append((y, x))

    lines_per_freq = {}
    for f, coords in antennas.items():
        lines_per_freq[f] = set()
        n = len(coords)
        for i in range(n):
            for j in range(i + 1, n):
                A = coords[i]
                B = coords[j]
                dy = B[0] - A[0]
                dx = B[1] - A[1]
                g = gcd(dy, dx)
                sy = dy // g
                sx = dx // g
                if sx < 0 or (sx == 0 and sy < 0):
                    sx = -sx
                    sy = -sy
                c = sy * A[1] - sx * A[0]
                lines_per_freq[f].add((sx, sy, c))

    antinodes = set()
    for lines in lines_per_freq.values():
        for sx, sy, c in lines:
            if sx == 0 and sy == 0:
                continue
            if sy == 0:
                if c % sx == 0:
                    y = -c // sx
                    if 0 <= y < h:
                        antinodes.update([(y, x) for x in range(w)])
            elif sx == 0:
                if c % sy == 0:
                    x = c // sy
                    if 0 <= x < w:
                        antinodes.update([(y, x) for y in range(h)])
            else:
                for y in range(h):
                    val = c + sx * y
                    if val % sy == 0:
                        x = val // sy
                        if 0 <= x < w:
                            antinodes.add((y, x))

    print(len(antinodes))

solve()
