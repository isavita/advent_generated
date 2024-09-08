def geologic_index(x, y, target, cache):
    if (x, y) in cache:
        return cache[(x, y)]
    
    if (x, y) == (0, 0) or (x, y) == target:
        result = 0
    elif y == 0:
        result = x * 16807
    elif x == 0:
        result = y * 48271
    else:
        result = geologic_index(x-1, y, target, cache) * geologic_index(x, y-1, target, cache)
    
    cache[(x, y)] = result
    return result

def erosion_level(x, y, target, depth, cache):
    if (x, y) not in cache:
        cache[(x, y)] = (geologic_index(x, y, target, cache) + depth) % 20183
    return cache[(x, y)]

def calculate_risk(depth, target):
    cache = {}
    risk = 0
    for y in range(target[1] + 1):
        for x in range(target[0] + 1):
            risk += erosion_level(x, y, target, depth, cache) % 3
    return risk

def main():
    depth = 11739
    target = (11, 718)
    print(f"Part 1: {calculate_risk(depth, target)}")

if __name__ == "__main__":
    main()
