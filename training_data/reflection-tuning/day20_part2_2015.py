def solve(target):
    limit = target // 10  # An upper bound for the house number
    houses = [0] * (limit + 1)
    
    # Part One
    for elf in range(1, limit + 1):
        for house in range(elf, limit + 1, elf):
            houses[house] += elf * 10
    
    for house, presents in enumerate(houses):
        if presents >= target:
            print(f"Part One: {house}")
            break
    
    # Reset houses array for Part Two
    houses = [0] * (limit + 1)
    
    # Part Two
    for elf in range(1, limit + 1):
        for house in range(elf, min(50 * elf + 1, limit + 1), elf):
            houses[house] += elf * 11
    
    for house, presents in enumerate(houses):
        if presents >= target:
            print(f"Part Two: {house}")
            break

# Read input from file
with open('input.txt', 'r') as file:
    target = int(file.read().strip())

solve(target)
