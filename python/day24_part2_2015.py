from itertools import combinations
from math import prod

# Step 1: Read the input from input.txt
with open('input.txt', 'r') as file:
    packages = [int(line.strip()) for line in file.readlines()]

# Step 2: Define a function to find the ideal grouping for four groups
def find_ideal_grouping_four(packages):
    total_weight = sum(packages)
    group_weight = total_weight // 4  # Adjusted for four groups
    for group_size in range(len(packages)):
        valid_first_groups = []
        for first_group in combinations(packages, group_size):
            if sum(first_group) == group_weight:
                remaining_packages_after_first = packages.copy()
                for item in first_group:
                    remaining_packages_after_first.remove(item)
                for second_group_size in range(1, len(remaining_packages_after_first)):
                    for second_group in combinations(remaining_packages_after_first, second_group_size):
                        if sum(second_group) == group_weight:
                            remaining_packages_after_second = remaining_packages_after_first.copy()
                            for item in second_group:
                                remaining_packages_after_second.remove(item)
                            for third_group in combinations(remaining_packages_after_second, len(remaining_packages_after_second) - second_group_size):
                                if sum(third_group) == group_weight:
                                    valid_first_groups.append(first_group)
                                    break
                            if valid_first_groups:
                                break
                    if valid_first_groups:
                        break
                if valid_first_groups:
                    break
        if valid_first_groups:
            return min(prod(group) for group in valid_first_groups)
    return None

# Step 3: Compute the quantum entanglement of the ideal configuration for four groups
quantum_entanglement = find_ideal_grouping_four(packages)

# Print the answer to the standard output
print(quantum_entanglement)
