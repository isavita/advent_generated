from collections import namedtuple
import re

Nanobot = namedtuple('Nanobot', ['x', 'y', 'z', 'r'])

def parse_input(input_data):
    pattern = re.compile(r'pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)')
    return [Nanobot(*map(int, pattern.match(line).groups())) for line in input_data.split('\n')]

def manhattan_distance(p1, p2):
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1]) + abs(p1[2] - p2[2])

def count_in_range(point, nanobots):
    return sum(1 for bot in nanobots if manhattan_distance(point, (bot.x, bot.y, bot.z)) <= bot.r)

def find_best_position(nanobots):
    # Find the bounding box of all nanobots
    min_x = min(bot.x - bot.r for bot in nanobots)
    max_x = max(bot.x + bot.r for bot in nanobots)
    min_y = min(bot.y - bot.r for bot in nanobots)
    max_y = max(bot.y + bot.r for bot in nanobots)
    min_z = min(bot.z - bot.r for bot in nanobots)
    max_z = max(bot.z + bot.r for bot in nanobots)

    # Start with a coarse grid
    step = max(max_x - min_x, max_y - min_y, max_z - min_z) // 10

    best_count = 0
    best_dist = float('inf')
    best_point = None

    while step > 0:
        for x in range(min_x, max_x + 1, step):
            for y in range(min_y, max_y + 1, step):
                for z in range(min_z, max_z + 1, step):
                    count = count_in_range((x, y, z), nanobots)
                    dist = manhattan_distance((x, y, z), (0, 0, 0))
                    if count > best_count or (count == best_count and dist < best_dist):
                        best_count = count
                        best_dist = dist
                        best_point = (x, y, z)

        # Refine the search around the best point
        min_x, max_x = best_point[0] - step, best_point[0] + step
        min_y, max_y = best_point[1] - step, best_point[1] + step
        min_z, max_z = best_point[2] - step, best_point[2] + step
        step //= 2

    return best_dist

def solve(input_data):
    nanobots = parse_input(input_data)
    
    # Part 1
    strongest = max(nanobots, key=lambda bot: bot.r)
    in_range = sum(1 for bot in nanobots if manhattan_distance((strongest.x, strongest.y, strongest.z), (bot.x, bot.y, bot.z)) <= strongest.r)
    
    # Part 2
    best_distance = find_best_position(nanobots)
    
    return in_range, best_distance

# Example usage:
# input_data = "... your input data here ..."
# part1, part2 = solve(input_data)
# print(f"Part 1: {part1}")
# print(f"Part 2: {part2}")
