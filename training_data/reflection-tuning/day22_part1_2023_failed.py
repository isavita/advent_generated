from collections import defaultdict

def parse_brick(line):
    start, end = line.split('~')
    return tuple(map(int, start.split(','))), tuple(map(int, end.split(',')))

def drop_bricks(bricks):
    bricks = sorted(bricks, key=lambda b: min(b[0][2], b[1][2]))
    fallen = {}
    for i, (start, end) in enumerate(bricks):
        x1, y1, z1 = start
        x2, y2, z2 = end
        max_z = 0
        for x in range(min(x1, x2), max(x1, x2) + 1):
            for y in range(min(y1, y2), max(y1, y2) + 1):
                for z in range(max_z + 1, 1000):
                    if (x, y, z) in fallen:
                        max_z = max(max_z, z)
                        break
        drop = z1 - (max_z + 1)
        new_start = (x1, y1, z1 - drop)
        new_end = (x2, y2, z2 - drop)
        for x in range(min(x1, x2), max(x1, x2) + 1):
            for y in range(min(y1, y2), max(y1, y2) + 1):
                for z in range(z1 - drop, z2 - drop + 1):
                    fallen[(x, y, z)] = i
        bricks[i] = (new_start, new_end)
    return bricks, fallen

def find_supports(bricks, fallen):
    supports = defaultdict(set)
    supported_by = defaultdict(set)
    for i, (start, end) in enumerate(bricks):
        x1, y1, z1 = start
        x2, y2, z2 = end
        for x in range(min(x1, x2), max(x1, x2) + 1):
            for y in range(min(y1, y2), max(y1, y2) + 1):
                below = fallen.get((x, y, z1 - 1), -1)
                if below != -1 and below != i:
                    supports[below].add(i)
                    supported_by[i].add(below)
    return supports, supported_by

def count_safe_removals(supports, supported_by):
    safe_count = 0
    for brick in range(len(supports)):
        if all(len(supported_by[supported]) > 1 for supported in supports[brick]):
            safe_count += 1
    return safe_count

def solve(input_data):
    bricks = [parse_brick(line) for line in input_data.strip().split('\n')]
    fallen_bricks, fallen = drop_bricks(bricks)
    supports, supported_by = find_supports(fallen_bricks, fallen)
    return count_safe_removals(supports, supported_by)

# Example usage:
input_data = """
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
"""
result = solve(input_data)
print(result)  # This should print the correct number of safely removable bricks
