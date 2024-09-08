from functools import lru_cache

def read_input():
    with open('input.txt', 'r') as f:
        depth = int(f.readline().split(': ')[1])
        target = tuple(map(int, f.readline().split(': ')[1].split(',')))
    return depth, target

@lru_cache(maxsize=None)
def geologic_index(x, y, target):
    if (x, y) in [(0, 0), target]:
        return 0
    if y == 0:
        return x * 16807
    if x == 0:
        return y * 48271
    return erosion_level(x-1, y, target) * erosion_level(x, y-1, target)

@lru_cache(maxsize=None)
def erosion_level(x, y, target):
    return (geologic_index(x, y, target) + depth) % 20183

def region_type(x, y, target):
    return erosion_level(x, y, target) % 3

def calculate_risk_level(target):
    risk_level = 0
    for y in range(target[1] + 1):
        for x in range(target[0] + 1):
            risk_level += region_type(x, y, target)
    return risk_level

depth, target = read_input()
total_risk = calculate_risk_level(target)
print(total_risk)
