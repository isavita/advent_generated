
import sys

def apply_insertion(polymer, rules):
    new_polymer = []
    for i in range(len(polymer)-1):
        new_polymer.append(polymer[i])
        if polymer[i:i+2] in rules:
            new_polymer.append(rules[polymer[i:i+2]])
    new_polymer.append(polymer[-1])
    return ''.join(new_polymer)

def count_elements(polymer):
    counts = {}
    for c in polymer:
        counts[c] = counts.get(c, 0) + 1
    return counts

def min_max(counts):
    min_count = sys.maxsize
    max_count = 0
    for count in counts.values():
        min_count = min(min_count, count)
        max_count = max(max_count, count)
    return min_count, max_count

if __name__ == '__main__':
    with open("input.txt", "r") as file:
        lines = file.readlines()
        polymer = lines[0].strip()
        rules = {}
        for line in lines[1:]:
            if line.strip():
                parts = line.strip().split(" -> ")
                rules[parts[0]] = parts[1]

    for _ in range(10):
        polymer = apply_insertion(polymer, rules)

    counts = count_elements(polymer)
    min_count, max_count = min_max(counts)

    print(max_count - min_count)
