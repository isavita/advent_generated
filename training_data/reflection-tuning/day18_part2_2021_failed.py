import ast
import itertools
from copy import deepcopy

def parse_input(filename):
    with open(filename, 'r') as f:
        return [ast.literal_eval(line.strip()) for line in f]

def add(a, b):
    return reduce([deepcopy(a), deepcopy(b)])

def explode(x, depth=0):
    if isinstance(x, int):
        return False, 0, x, 0
    if depth == 4:
        return True, x[0], 0, x[1]
    a, b = x
    exp, left, a, right = explode(a, depth + 1)
    if exp:
        return True, left, [a, b + right], 0
    exp, left, b, right = explode(b, depth + 1)
    if exp:
        return True, 0, [a + left, b], right
    return False, 0, x, 0

def split(x):
    if isinstance(x, int):
        if x >= 10:
            return True, [x // 2, (x + 1) // 2]
        return False, x
    a, b = x
    changed, a = split(a)
    if changed:
        return True, [a, b]
    changed, b = split(b)
    return changed, [a, b]

def reduce(x):
    while True:
        changed, _, x, _ = explode(x)
        if changed:
            continue
        changed, x = split(x)
        if not changed:
            break
    return x

def magnitude(x):
    if isinstance(x, int):
        return x
    return 3 * magnitude(x[0]) + 2 * magnitude(x[1])

def part1(numbers):
    result = numbers[0]
    for num in numbers[1:]:
        result = add(result, num)
    return magnitude(result)

def part2(numbers):
    return max(magnitude(add(a, b)) for a, b in itertools.permutations(numbers, 2))

numbers = parse_input('input.txt')
print(f"Part 1: {part1(numbers)}")
print(f"Part 2: {part2(numbers)}")
