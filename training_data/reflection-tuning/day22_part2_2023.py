from collections import defaultdict

class Brick:
    def __init__(self, start, end):
        self.start = start
        self.end = end
        self.cubes = set(self.get_cubes())

    def get_cubes(self):
        x1, y1, z1 = self.start
        x2, y2, z2 = self.end
        return [(x, y, z) for x in range(min(x1, x2), max(x1, x2) + 1)
                          for y in range(min(y1, y2), max(y1, y2) + 1)
                          for z in range(min(z1, z2), max(z1, z2) + 1)]

    def move_down(self):
        self.start = (self.start[0], self.start[1], self.start[2] - 1)
        self.end = (self.end[0], self.end[1], self.end[2] - 1)
        self.cubes = set((x, y, z - 1) for x, y, z in self.cubes)

def parse_input(filename):
    bricks = []
    with open(filename, 'r') as file:
        for line in file:
            start, end = line.strip().split('~')
            start = tuple(map(int, start.split(',')))
            end = tuple(map(int, end.split(',')))
            bricks.append(Brick(start, end))
    return sorted(bricks, key=lambda b: min(b.start[2], b.end[2]))

def fall_bricks(bricks):
    occupied = set()
    for brick in bricks:
        while min(cube[2] for cube in brick.cubes) > 1 and not any((x, y, z - 1) in occupied for x, y, z in brick.cubes):
            brick.move_down()
        occupied.update(brick.cubes)

def build_support_graphs(bricks):
    supports = defaultdict(set)
    supported_by = defaultdict(set)
    occupied = {}
    
    for i, brick in enumerate(bricks):
        for x, y, z in brick.cubes:
            if (x, y, z - 1) in occupied and occupied[(x, y, z - 1)] != i:
                supports[occupied[(x, y, z - 1)]].add(i)
                supported_by[i].add(occupied[(x, y, z - 1)])
            occupied[(x, y, z)] = i
    
    return supports, supported_by

def count_falling_bricks(i, supports, supported_by):
    falling = set([i])
    to_check = list(supports[i])
    
    while to_check:
        brick = to_check.pop()
        if supported_by[brick].issubset(falling):
            falling.add(brick)
            to_check.extend(supports[brick])
    
    return len(falling) - 1  # Subtract 1 to exclude the brick itself

def solve(filename):
    bricks = parse_input(filename)
    fall_bricks(bricks)
    supports, supported_by = build_support_graphs(bricks)
    
    total_falling = sum(count_falling_bricks(i, supports, supported_by) for i in range(len(bricks)))
    
    print(f"The sum of the number of other bricks that would fall is: {total_falling}")

solve("input.txt")
