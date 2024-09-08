def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)

def modinv(a, m):
    g, x, y = egcd(a, m)
    if g != 1:
        raise Exception('modular inverse does not exist')
    else:
        return x % m

def compose(f, g, m):
    a, b = f
    c, d = g
    return ((a * c) % m, (a * d + b) % m)

def power(f, n, m):
    if n == 0:
        return (1, 0)
    elif n % 2 == 0:
        return power(compose(f, f, m), n // 2, m)
    else:
        return compose(f, power(f, n - 1, m), m)

def parse_shuffle(lines, m):
    f = (1, 0)
    for line in lines:
        if line.startswith("deal into new stack"):
            f = compose((-1, -1), f, m)
        elif line.startswith("cut"):
            n = int(line.split()[-1])
            f = compose((1, -n), f, m)
        elif line.startswith("deal with increment"):
            n = int(line.split()[-1])
            f = compose((n, 0), f, m)
    return f

def solve_part1(lines):
    m = 10007
    f = parse_shuffle(lines, m)
    return (f[0] * 2019 + f[1]) % m

def solve_part2(lines):
    m = 119315717514047
    n = 101741582076661
    f = parse_shuffle(lines, m)
    f_n = power(f, n, m)
    a, b = f_n
    inv_a = modinv(a, m)
    return ((2020 - b) * inv_a) % m

with open('input.txt', 'r') as file:
    lines = file.readlines()

print("Part 1:", solve_part1(lines))
print("Part 2:", solve_part2(lines))
