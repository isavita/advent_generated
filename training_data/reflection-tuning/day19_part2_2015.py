import re
from collections import defaultdict
import random

def parse_input(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    replacements = defaultdict(list)
    for line in lines:
        if '=>' in line:
            left, right = line.strip().split(' => ')
            replacements[left].append(right)
        elif line.strip():
            medicine = line.strip()
    
    return replacements, medicine

def part1(replacements, medicine):
    molecules = set()
    for left, rights in replacements.items():
        for right in rights:
            for match in re.finditer(re.escape(left), medicine):
                new_molecule = medicine[:match.start()] + right + medicine[match.end():]
                molecules.add(new_molecule)
    return len(molecules)

def part2(replacements, medicine):
    target = medicine
    steps = 0
    inv_replacements = [(r, l) for l, rights in replacements.items() for r in rights]
    
    while target != 'e':
        tmp = target
        for r, l in random.sample(inv_replacements, len(inv_replacements)):
            if r in target:
                target = target.replace(r, l, 1)
                steps += 1
        if tmp == target:
            target = medicine
            steps = 0
    
    return steps

replacements, medicine = parse_input('input.txt')
print(f"Part 1: {part1(replacements, medicine)}")
print(f"Part 2: {part2(replacements, medicine)}")
