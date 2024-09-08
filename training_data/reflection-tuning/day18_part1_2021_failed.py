from math import ceil, floor

def explode(x, depth=0):
    if isinstance(x, int):
        return False, x, 0, 0
    if depth == 4:
        return True, 0, x[0], x[1]
    a, b = x
    exp, a, left, right = explode(a, depth + 1)
    if exp:
        return True, [a, b + right] if isinstance(b, int) else [a, [b[0] + right, b[1]]], left, 0
    exp, b, left, right = explode(b, depth + 1)
    if exp:
        return True, [a + left, b] if isinstance(a, int) else [[a[0], a[1] + left], b], 0, right
    return False, [a, b], 0, 0

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
        changed, x, _, _ = explode(x)
        if changed:
            continue
        changed, x = split(x)
        if not changed:
            break
    return x

def add(a, b):
    return reduce([a, b])

def magnitude(x):
    if isinstance(x, int):
        return x
    return 3 * magnitude(x[0]) + 2 * magnitude(x[1])

def solve(numbers):
    result = numbers[0]
    for num in numbers[1:]:
        result = add(result, num)
    return magnitude(result)

def parse(s):
    return eval(s)

# Read input
with open('input.txt', 'r') as f:
    numbers = [parse(line.strip()) for line in f]

print(solve(numbers))
