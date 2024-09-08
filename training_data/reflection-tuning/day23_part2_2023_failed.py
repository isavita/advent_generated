from collections import deque

def parse_input(data):
    grid = [list(line.strip()) for line in data.split('\n')]
    start = (0, grid[0].index('.'))
    end = (len(grid) - 1, grid[-1].index('.'))
    return grid, start, end

def longest_hike(grid, start, end, ignore_slopes=False):
    rows, cols = len(grid), len(grid[0])
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    slope_dirs = {'>': (0, 1), 'v': (1, 0), '<': (0, -1), '^': (-1, 0)}

    def get_neighbors(x, y):
        if not ignore_slopes and grid[x][y] in slope_dirs:
            dx, dy = slope_dirs[grid[x][y]]
            return [(x + dx, y + dy)]
        return [(x + dx, y + dy) for dx, dy in directions]

    stack = [(start, set([start]))]
    max_length = 0

    while stack:
        (x, y), path = stack.pop()

        if (x, y) == end:
            max_length = max(max_length, len(path) - 1)
            continue

        for nx, ny in get_neighbors(x, y):
            if 0 <= nx < rows and 0 <= ny < cols and grid[nx][ny] != '#' and (nx, ny) not in path:
                new_path = path.copy()
                new_path.add((nx, ny))
                stack.append(((nx, ny), new_path))

    return max_length

def solve(data):
    grid, start, end = parse_input(data)
    part1 = longest_hike(grid, start, end)
    part2 = longest_hike(grid, start, end, ignore_slopes=True)
    return part1, part2

# Example usage:
sample_input = """#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#"""

part1, part2 = solve(sample_input)
print("Part 1:", part1)
print("Part 2:", part2)
