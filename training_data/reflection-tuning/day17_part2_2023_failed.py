import heapq

def parse_input(input_str):
    return [[int(c) for c in line.strip()] for line in input_str.split('\n')]

def find_least_heat_loss(grid, min_straight, max_straight):
    rows, cols = len(grid), len(grid[0])
    goal = (rows - 1, cols - 1)
    
    # (heat_loss, row, col, direction, steps_in_direction)
    queue = [(0, 0, 0, (0, 1), 0), (0, 0, 0, (1, 0), 0)]
    visited = set()

    while queue:
        heat_loss, row, col, direction, steps = heapq.heappop(queue)

        if (row, col) == goal and steps >= min_straight:
            return heat_loss

        if (row, col, direction, steps) in visited:
            continue
        visited.add((row, col, direction, steps))

        if steps < max_straight:
            new_row, new_col = row + direction[0], col + direction[1]
            if 0 <= new_row < rows and 0 <= new_col < cols:
                new_heat_loss = heat_loss + grid[new_row][new_col]
                heapq.heappush(queue, (new_heat_loss, new_row, new_col, direction, steps + 1))

        if steps >= min_straight:
            for new_direction in [(0, 1), (1, 0), (0, -1), (-1, 0)]:
                if new_direction != direction and new_direction != (-direction[0], -direction[1]):
                    new_row, new_col = row + new_direction[0], col + new_direction[1]
                    if 0 <= new_row < rows and 0 <= new_col < cols:
                        new_heat_loss = heat_loss + grid[new_row][new_col]
                        heapq.heappush(queue, (new_heat_loss, new_row, new_col, new_direction, 1))

    return float('inf')

def solve(input_str):
    grid = parse_input(input_str)
    part1 = find_least_heat_loss(grid, 0, 3)
    part2 = find_least_heat_loss(grid, 4, 10)
    return part1, part2

# Example usage:
input_str = """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"""

part1, part2 = solve(input_str)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
