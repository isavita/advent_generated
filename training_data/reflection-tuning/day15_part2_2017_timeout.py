def read_input(filename):
    with open(filename, 'r') as f:
        return [int(line.split()[-1]) for line in f]

def generator(prev, factor, mod=1):
    while True:
        prev = (prev * factor) % 2147483647
        if prev % mod == 0:
            yield prev

def count_matches(gen_a, gen_b, pairs):
    return sum((next(gen_a) & 0xFFFF) == (next(gen_b) & 0xFFFF) for _ in range(pairs))

def solve(start_a, start_b):
    # Part 1
    gen_a = generator(start_a, 16807)
    gen_b = generator(start_b, 48271)
    part1 = count_matches(gen_a, gen_b, 40_000_000)
    print(f"Part 1: {part1}")

    # Part 2
    gen_a = generator(start_a, 16807, 4)
    gen_b = generator(start_b, 48271, 8)
    part2 = count_matches(gen_a, gen_b, 5_000_000)
    print(f"Part 2: {part2}")

if __name__ == "__main__":
    start_a, start_b = read_input("input.txt")
    solve(start_a, start_b)
