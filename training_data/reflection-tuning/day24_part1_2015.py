from itertools import combinations
from functools import reduce
from operator import mul
from functools import lru_cache

def read_input(filename):
    with open(filename, 'r') as f:
        return [int(line.strip()) for line in f]

@lru_cache(maxsize=None)
def find_combinations(weights, target, max_length):
    result = []
    for i in range(1, max_length + 1):
        for combo in combinations(weights, i):
            if sum(combo) == target:
                result.append(combo)
        if result:
            return result
    return result

def quantum_entanglement(combo):
    return reduce(mul, combo)

def solve(weights):
    total_weight = sum(weights)
    target_weight = total_weight // 3

    all_combinations = find_combinations(tuple(weights), target_weight, len(weights) // 3)
    
    if not all_combinations:
        return None

    sorted_combinations = sorted(all_combinations, key=lambda x: (len(x), quantum_entanglement(x)))
    
    return quantum_entanglement(sorted_combinations[0])

def main():
    weights = read_input("input.txt")
    result = solve(weights)
    print(result)

if __name__ == "__main__":
    main()
