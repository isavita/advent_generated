def mix(numbers, times=1):
    n = len(numbers)
    indices = list(range(n))
    
    for _ in range(times):
        for i in range(n):
            idx = indices.index(i)
            value = numbers[i]
            new_idx = (idx + value) % (n - 1)
            indices.pop(idx)
            indices.insert(new_idx, i)
    
    return [numbers[i] for i in indices]

def solve(numbers, part2=False):
    if part2:
        numbers = [x * 811589153 for x in numbers]
        numbers = mix(numbers, 10)
    else:
        numbers = mix(numbers)
    
    zero_pos = numbers.index(0)
    return sum(numbers[(zero_pos + i) % len(numbers)] for i in [1000, 2000, 3000])

with open('input.txt', 'r') as f:
    numbers = [int(line.strip()) for line in f]

print("Part 1:", solve(numbers))
print("Part 2:", solve(numbers, True))
