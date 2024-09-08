def find_lowest_house_number(target):
    max_houses = target // 10  # Upper bound estimation
    presents = [0] * (max_houses + 1)
    
    for elf in range(1, max_houses + 1):
        for house in range(elf, max_houses + 1, elf):
            presents[house] += elf * 10
            if presents[house] >= target:
                return house

# Read input from file
with open('input.txt', 'r') as file:
    target_presents = int(file.read().strip())

# Find and print the answer
result = find_lowest_house_number(target_presents)
print(result)
