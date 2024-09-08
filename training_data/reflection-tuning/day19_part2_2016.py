import math

def solve_part_one(n):
    return 2 * (n - 2**int(math.log2(n))) + 1

def solve_part_two(n):
    if n == 1:
        return 1
    
    power_of_3 = 3 ** int(math.log(n, 3))
    if n == power_of_3:
        return 1
    elif n <= 2 * power_of_3:
        return n - power_of_3
    else:
        return 2 * n - 3 * power_of_3

# Read input from file
with open("input.txt", "r") as file:
    num_elves = int(file.read().strip())

# Solve and print results
print(f"Part One: {solve_part_one(num_elves)}")
print(f"Part Two: {solve_part_two(num_elves)}")
