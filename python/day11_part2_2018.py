def calculate_power_level(x, y, serial_number):
    rack_id = x + 10
    power_level = rack_id * y
    power_level += serial_number
    power_level *= rack_id
    power_level = (power_level // 100) % 10
    power_level -= 5
    return power_level

def calculate_cumulative_grid(grid):
    cumulative_grid = [[0] * 301 for _ in range(301)]
    for x in range(1, 301):
        for y in range(1, 301):
            cumulative_grid[x][y] = (
                grid[x - 1][y - 1]
                + cumulative_grid[x - 1][y]
                + cumulative_grid[x][y - 1]
                - cumulative_grid[x - 1][y - 1]
            )
    return cumulative_grid

def calculate_total_power(cumulative_grid, x, y, size):
    x1, y1 = x - 1, y - 1
    x2, y2 = x + size - 1, y + size - 1
    total_power = (
        cumulative_grid[x2][y2]
        - cumulative_grid[x1][y2]
        - cumulative_grid[x2][y1]
        + cumulative_grid[x1][y1]
    )
    return total_power

def find_largest_square(grid, cumulative_grid):
    max_power = float('-inf')
    max_coordinates = None
    for size in range(1, 301):
        for x in range(1, 302 - size):
            for y in range(1, 302 - size):
                total_power = calculate_total_power(cumulative_grid, x, y, size)
                if total_power > max_power:
                    max_power = total_power
                    max_coordinates = (x, y, size)
    return max_coordinates

def main():
    with open('input.txt', 'r') as file:
        serial_number = int(file.read().strip())
    
    grid = [[0] * 300 for _ in range(300)]
    for x in range(300):
        for y in range(300):
            grid[x][y] = calculate_power_level(x + 1, y + 1, serial_number)
    
    cumulative_grid = calculate_cumulative_grid(grid)
    max_coordinates = find_largest_square(grid, cumulative_grid)
    
    print(f"{max_coordinates[0]},{max_coordinates[1]},{max_coordinates[2]}")

if __name__ == '__main__':
    main()
