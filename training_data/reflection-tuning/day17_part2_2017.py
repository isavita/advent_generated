def solve_part_one(steps):
    buffer = [0]
    position = 0
    for i in range(1, 2018):
        position = (position + steps) % len(buffer) + 1
        buffer.insert(position, i)
    return buffer[(buffer.index(2017) + 1) % len(buffer)]

def solve_part_two(steps):
    position = 0
    value_after_zero = 0
    for i in range(1, 50000001):
        position = (position + steps) % i + 1
        if position == 1:
            value_after_zero = i
    return value_after_zero

# Read input from file
with open('input.txt', 'r') as file:
    steps = int(file.read().strip())

# Solve and print results
print("Part One:", solve_part_one(steps))
print("Part Two:", solve_part_two(steps))
