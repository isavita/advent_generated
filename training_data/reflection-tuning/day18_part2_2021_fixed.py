from copy import deepcopy
from math import ceil, floor

def explode(x, depth=0):
    if isinstance(x, int):
        return False, 0, x, 0
    if depth == 4:
        return True, x[0], 0, x[1]
    a, b = x
    exp, left, a, right = explode(a, depth + 1)
    if exp:
        if isinstance(b, int):
            return True, left, [a, b + right], 0
        else:
            return True, left, [a, add_left(b, right)], 0
    exp, left, b, right = explode(b, depth + 1)
    if exp:
        if isinstance(a, int):
            return True, 0, [a + left, b], right
        else:
            return True, 0, [add_right(a, left), b], right
    return False, 0, x, 0

def add_left(x, n):
    if isinstance(x, int):
        return x + n
    return [add_left(x[0], n), x[1]]

def add_right(x, n):
    if isinstance(x, int):
        return x + n
    return [x[0], add_right(x[1], n)]

def split(x):
    if isinstance(x, int):
        if x >= 10:
            return True, [floor(x / 2), ceil(x / 2)]
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

def add(a, b):
    return reduce([deepcopy(a), deepcopy(b)])

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
    return max(magnitude(add(a, b)) for i, a in enumerate(numbers) for j, b in enumerate(numbers) if i != j)

# Read input
with open('input.txt', 'r') as f:
    numbers = [eval(line.strip()) for line in f]

print(f"Part 1: {part1(numbers)}")
print(f"Part 2: {part2(numbers)}")
