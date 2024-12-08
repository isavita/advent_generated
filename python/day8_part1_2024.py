
def solve():
    with open("input.txt") as f:
        grid = f.readlines()

    h = len(grid)
    w = len(grid[0]) -1 # Account for newline character

    antennas = {}
    for y, row in enumerate(grid):
        for x, c in enumerate(row.strip()):
            if c != '.':
                antennas.setdefault(c, []).append((y, x))

    antinodes = set()
    for coords in antennas.values():
        for i in range(len(coords)):
            for j in range(i + 1, len(coords)):
                A = coords[i]
                B = coords[j]
                P1 = (2 * A[0] - B[0], 2 * A[1] - B[1])
                P2 = (2 * B[0] - A[0], 2 * B[1] - A[1])
                if 0 <= P1[0] < h and 0 <= P1[1] < w:
                    antinodes.add(P1)
                if 0 <= P2[0] < h and 0 <= P2[1] < w:
                    antinodes.add(P2)

    print(len(antinodes))

solve()
