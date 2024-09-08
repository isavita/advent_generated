def generator(start, factor, divisor=2147483647):
    value = start
    while True:
        value = (value * factor) % divisor
        yield value

def compare_lowest_16_bits(a, b):
    return (a & 0xFFFF) == (b & 0xFFFF)

def solve(start_a, start_b):
    gen_a = generator(start_a, 16807)
    gen_b = generator(start_b, 48271)
    
    count = sum(compare_lowest_16_bits(next(gen_a), next(gen_b))
                for _ in range(40_000_000))
    
    return count

# Read input from file
with open('input.txt', 'r') as f:
    start_a = int(f.readline().split()[-1])
    start_b = int(f.readline().split()[-1])

# Solve and print the result
result = solve(start_a, start_b)
print(result)
