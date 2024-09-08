from collections import defaultdict

def parse_input(filename):
    steps = []
    with open(filename, 'r') as f:
        for line in f:
            state, coords = line.strip().split()
            ranges = [list(map(int, c[2:].split('..'))) for c in coords.split(',')]
            steps.append((state == 'on', ranges))
    return steps

def compress_coordinates(steps):
    coords = defaultdict(set)
    for _, ranges in steps:
        for i, (start, end) in enumerate(ranges):
            coords[i].add(start)
            coords[i].add(end + 1)
    return [sorted(coords[i]) for i in range(3)]

def coordinate_map(compressed):
    return {v: i for i, v in enumerate(compressed)}

def update_reactor(reactor, state, ranges, maps):
    x1, x2 = ranges[0]
    y1, y2 = ranges[1]
    z1, z2 = ranges[2]
    for x in range(maps[0][x1], maps[0][x2 + 1]):
        for y in range(maps[1][y1], maps[1][y2 + 1]):
            for z in range(maps[2][z1], maps[2][z2 + 1]):
                reactor[x][y][z] = state

def calculate_volume(reactor, compressed):
    total = 0
    for x in range(len(compressed[0]) - 1):
        for y in range(len(compressed[1]) - 1):
            for z in range(len(compressed[2]) - 1):
                if reactor[x][y][z]:
                    dx = compressed[0][x + 1] - compressed[0][x]
                    dy = compressed[1][y + 1] - compressed[1][y]
                    dz = compressed[2][z + 1] - compressed[2][z]
                    total += dx * dy * dz
    return total

def solve(steps):
    compressed = compress_coordinates(steps)
    maps = [coordinate_map(c) for c in compressed]
    reactor = [[[False for _ in range(len(compressed[2]))] 
                for _ in range(len(compressed[1]))] 
                for _ in range(len(compressed[0]))]

    for state, ranges in steps:
        update_reactor(reactor, state, ranges, maps)

    return calculate_volume(reactor, compressed)

steps = parse_input('input.txt')
result = solve(steps)
print(result)
