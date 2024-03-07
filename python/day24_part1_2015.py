from itertools import combinations
from math import prod

# Step 1: Read the input from input.txt
with open('input.txt', 'r') as file:
    packages = [int(line.strip()) for line in file.readlines()]

# Step 2: Define a function to find the ideal grouping
def find_ideal_grouping(packages):
    total_weight = sum(packages)
    group_weight = total_weight // 3
    for group_size in range(len(packages)):
        valid_groups = []
        for combo in combinations(packages, group_size):
            if sum(combo) == group_weight:
                remaining_packages = packages.copy()
                for item in combo:
                    remaining_packages.remove(item)
                for second_group_size in range(len(remaining_packages)):
                    for second_combo in combinations(remaining_packages, second_group_size):
                        if sum(second_combo) == group_weight:
                            valid_groups.append(combo)
                            break
                    if valid_groups:
                        break
                if valid_groups:
                    break
        if valid_groups:
            return min(prod(group) for group in valid_groups)
    return None

# Step 3: Compute the quantum entanglement of the ideal configuration
quantum_entanglement = find_ideal_grouping(packages)

# Print the answer to the standard output
print(quantum_entanglement)

