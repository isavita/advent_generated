import math

def parse_data(data):
    garden = {}
    start = None
    for y, line in enumerate(data):
        for x, c in enumerate(line):
            if c != '#':
                garden[complex(x, y)] = True
            if c == 'S':
                start = complex(x, y)
    if start is None:
        raise ValueError("No start found!")
    return garden, start

def complex_mod(num, mod):
    if not num.real.is_integer() or not num.imag.is_integer():
        raise ValueError(f"Complex number {num} is not integer!")
    return complex((int(num.real) + 10 * mod) % mod, (int(num.imag) + 10 * mod) % mod)

def calculate_num_ends(garden, start, num_iterations, max_size):
    queue = {start: True}
    done = []

    for i in range(3 * max_size):
        if (i % max_size) == (max_size - 1) // 2:
            done.append(len(queue))
        if len(done) == 3:
            break

        new_queue = {}
        for point in queue:
            for dir in [1, -1, 1j, -1j]:
                if complex_mod(point + dir, max_size) in garden:
                    new_queue[point + dir] = True
        queue = new_queue

    def quadratic_function(n, a, b, c):
        return a + n * (b - a + ((n - 1) * (c - 2 * b + a) // 2))

    return quadratic_function(num_iterations // max_size, done[0], done[1], done[2])

with open("input.txt", "r") as file:
    garden_input = [line.strip() for line in file.readlines()]

garden, start = parse_data(garden_input)
max_size = len(garden_input)
print(calculate_num_ends(garden, start, 26501365, max_size))