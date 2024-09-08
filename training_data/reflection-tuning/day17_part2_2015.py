def solve_eggnog_problem(containers, target=150):
    n = len(containers)
    total_combinations = 0
    min_containers = float('inf')
    min_container_combinations = 0

    for mask in range(1, 1 << n):
        volume = sum(containers[i] for i in range(n) if mask & (1 << i))
        if volume == target:
            total_combinations += 1
            container_count = bin(mask).count('1')
            if container_count < min_containers:
                min_containers = container_count
                min_container_combinations = 1
            elif container_count == min_containers:
                min_container_combinations += 1

    return total_combinations, min_container_combinations

# Read input from file
with open('input.txt', 'r') as file:
    containers = [int(line.strip()) for line in file]

# Solve the problem
part1, part2 = solve_eggnog_problem(containers)

# Print results
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
