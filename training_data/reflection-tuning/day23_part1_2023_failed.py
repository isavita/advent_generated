def longest_hike(grid):
    rows, cols = len(grid), len(grid[0])
    start = (0, grid[0].index('.'))
    end = (rows - 1, grid[-1].index('.'))
    
    directions = {'^': (-1, 0), 'v': (1, 0), '<': (0, -1), '>': (0, 1)}
    all_directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

    def is_valid(x, y):
        return 0 <= x < rows and 0 <= y < cols and grid[x][y] != '#'

    def dfs(x, y):
        stack = [(x, y, 0, set())]
        max_length = 0

        while stack:
            x, y, length, visited = stack.pop()
            
            if (x, y) == end:
                max_length = max(max_length, length)
                continue

            visited.add((x, y))

            if grid[x][y] in directions:
                dx, dy = directions[grid[x][y]]
                nx, ny = x + dx, y + dy
                if is_valid(nx, ny) and (nx, ny) not in visited:
                    stack.append((nx, ny, length + 1, visited.copy()))
            else:
                for dx, dy in all_directions:
                    nx, ny = x + dx, y + dy
                    if is_valid(nx, ny) and (nx, ny) not in visited:
                        stack.append((nx, ny, length + 1, visited.copy()))

        return max_length

    return dfs(start[0], start[1])

# Example usage:
grid = [
    "#.#####################",
    "#.......#########...###",
    "#######.#########.#.###",
    "###.....#.>.>.###.#.###",
    "###v#####.#v#.###.#.###",
    "###.>...#.#.#.....#...#",
    "###v###.#.#.#########.#",
    "###...#.#.#.......#...#",
    "#####.#.#.#######.#.###",
    "#.....#.#.#.......#...#",
    "#.#####.#.#.#########v#",
    "#.#...#...#...###...>.#",
    "#.#.#v#######v###.###v#",
    "#...#.>.#...>.>.#.###.#",
    "#####v#.#.###v#.#.###.#",
    "#.....#...#...#.#.#...#",
    "#.#########.###.#.#.###",
    "#...###...#...#...#.###",
    "###.###.#.###v#####v###",
    "#...#...#.#.>.>.#.>.###",
    "#.###.###.#.###.#.#v###",
    "#.....###...###...#...#",
    "#####################.#"
]

print(longest_hike(grid))
