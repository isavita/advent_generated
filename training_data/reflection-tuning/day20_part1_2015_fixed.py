def find_lowest_house_number(target_presents):
    limit = target_presents // 10  # We can use this as an upper bound
    houses = [0] * (limit + 1)
    
    for elf in range(1, limit + 1):
        for house in range(elf, limit + 1, elf):
            houses[house] += elf * 10
    
    for house, presents in enumerate(houses):
        if presents >= target_presents:
            return house

# Example usage
target = 29000000  # Replace with your puzzle input
result = find_lowest_house_number(target)
print(result)
