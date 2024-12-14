
import re

def mod(a, b):
    return (a % b + b) % b

def parse_line(line):
    match = re.match(r'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)', line)
    if not match:
        raise ValueError(f"Invalid line format: {line}")
    x, y, vx, vy = map(int, match.groups())
    return (x, y, vx, vy)

def move_robots(robots, size_x, size_y):
    return [(mod(x + vx, size_x), mod(y + vy, size_y), vx, vy) for x, y, vx, vy in robots]

def count_quadrants(robots, size_x, size_y):
    counts = [0, 0, 0, 0]
    center_x = size_x // 2
    center_y = size_y // 2
    for x, y, _, _ in robots:
        if x < center_x:
            if y < center_y:
                counts[0] += 1
            elif y > center_y:
                counts[1] += 1
        elif x > center_x:
            if y < center_y:
                counts[2] += 1
            elif y > center_y:
                counts[3] += 1
    return counts

def has_no_overlaps(robots):
    positions = set()
    for x, y, _, _ in robots:
        if (x, y) in positions:
            return False
        positions.add((x, y))
    return True

def draw_grid(robots, size_x, size_y):
    grid = [['.' for _ in range(size_x)] for _ in range(size_y)]
    for x, y, _, _ in robots:
        grid[y][x] = '#'
    for row in grid:
        print(''.join(row))

def main():
    size_x = 101
    size_y = 103
    robots = []
    with open("input.txt", "r") as f:
        for line in f:
            line = line.strip()
            if line:
                robots.append(parse_line(line))

    robots_part1 = robots[:]
    for _ in range(100):
        robots_part1 = move_robots(robots_part1, size_x, size_y)
    counts = count_quadrants(robots_part1, size_x, size_y)
    safety_factor = 1
    for c in counts:
        safety_factor *= c
    print(f"Part 1 - Safety Factor after 100 seconds: {safety_factor}")

    robots_part2 = robots[:]
    seconds = 0
    while True:
        if has_no_overlaps(robots_part2):
            break
        robots_part2 = move_robots(robots_part2, size_x, size_y)
        seconds += 1
        if seconds > 1000000:
            print("Exceeded maximum iterations without finding a unique position configuration.")
            return
    print(f"Part 2 - Fewest seconds to display Easter egg: {seconds}")
    print("Final positions of robots:")
    draw_grid(robots_part2, size_x, size_y)

if __name__ == "__main__":
    main()
