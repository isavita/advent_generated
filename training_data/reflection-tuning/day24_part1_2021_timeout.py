import re
from functools import lru_cache

def parse_input(filename):
    with open(filename, 'r') as file:
        instructions = file.read().strip().split('inp w\n')[1:]
    params = []
    for block in instructions:
        a, b, c = re.findall(r'add x (-?\d+).*add y (-?\d+).*add y (-?\d+)', block, re.DOTALL)[0]
        params.append((int(a), int(b), int(c)))
    return params

@lru_cache(maxsize=None)
def solve(digit, z, params):
    a, b, c = params
    x = (z % 26) + a != digit
    z = (z // b) * (25 * x + 1) + (digit + c) * x
    return z

@lru_cache(maxsize=None)
def find_largest(index, z, params):
    if index == 14:
        return '' if z == 0 else None
    for digit in range(9, 0, -1):
        new_z = solve(digit, z, params[index])
        result = find_largest(index + 1, new_z, params)
        if result is not None:
            return str(digit) + result
    return None

def main():
    params = tuple(parse_input('input.txt'))
    result = find_largest(0, 0, params)
    print(result)

if __name__ == "__main__":
    main()
