from itertools import combinations
from functools import reduce
from operator import mul

def can_be_divided(packages, group, target_sum, groups_left):
    if groups_left == 1:
        return sum(packages) == target_sum
    if sum(group) > target_sum:
        return False
    if sum(group) == target_sum:
        return can_be_divided(packages, [], target_sum, groups_left - 1)
    for i, p in enumerate(packages):
        if can_be_divided(packages[i+1:], group + [p], target_sum, groups_left):
            return True
    return False

def find_optimal_configuration(weights, num_groups):
    total_weight = sum(weights)
    target_weight = total_weight // num_groups
    
    for group_size in range(1, len(weights)):
        valid_groups = []
        for group in combinations(weights, group_size):
            if sum(group) == target_weight:
                remaining = [w for w in weights if w not in group]
                if can_be_divided(remaining, [], target_weight, num_groups - 1):
                    valid_groups.append(group)
        
        if valid_groups:
            return min(valid_groups, key=lambda g: reduce(mul, g))
    
    return None

def solve(weights, num_groups):
    optimal_group = find_optimal_configuration(weights, num_groups)
    return reduce(mul, optimal_group) if optimal_group else None

# Read input
with open('input.txt', 'r') as f:
    weights = [int(line.strip()) for line in f]

# Solve Part Two
result = solve(weights, 4)
print(result)
