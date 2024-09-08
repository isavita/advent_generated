def egcd(a, b):
    if a == 0:
        return b, 0, 1
    else:
        g, y, x = egcd(b % a, a)
        return g, x - (b // a) * y, y

def modinv(a, m):
    g, x, _ = egcd(a, m)
    if g != 1:
        raise Exception('Modular inverse does not exist')
    else:
        return x % m

def compose(f, g, m):
    a, b = f
    c, d = g
    return (a * c % m, (a * d + b) % m)

def apply(f, x, m):
    a, b = f
    return (a * x + b) % m

def power(f, n, m):
    if n == 0:
        return (1, 0)
    elif n % 2 == 0:
        return power(compose(f, f, m), n // 2, m)
    else:
        return compose(f, power(f, n - 1, m), m)

def parse_shuffle(line):
    if line.startswith("deal into new stack"):
        return (-1, -1)
    elif line.startswith("cut"):
        n = int(line.split()[-1])
        return (1, -n)
    elif line.startswith("deal with increment"):
        n = int(line.split()[-1])
        return (n, 0)

def solve(lines, deck_size, iterations=1, position=None):
    f = (1, 0)
    for line in lines:
        f = compose(f, parse_shuffle(line), deck_size)
    
    if iterations > 1:
        f = power(f, iterations, deck_size)
    
    if position is None:
        return apply(f, 2019, deck_size)
    else:
        a, b = f
        return ((position - b) * modinv(a, deck_size)) % deck_size

with open("input.txt", "r") as file:
    lines = file.readlines()

# Part 1
print(solve(lines, 10007))

# Part 2
print(solve(lines, 119315717514047, 101741582076661, 2020))
