def read_input(filename):
    with open(filename, 'r') as file:
        return ''.join(line.strip() for line in file)

def count_adjacent_bugs(layout, i):
    adjacent = [i-5, i+5, i-1 if i%5 != 0 else -1, i+1 if i%5 != 4 else -1]
    return sum(0 <= j < 25 and layout[j] == '#' for j in adjacent)

def evolve(layout):
    new_layout = ''
    for i, tile in enumerate(layout):
        adjacent_bugs = count_adjacent_bugs(layout, i)
        if tile == '#':
            new_layout += '#' if adjacent_bugs == 1 else '.'
        else:
            new_layout += '#' if 1 <= adjacent_bugs <= 2 else '.'
    return new_layout

def calculate_biodiversity(layout):
    return sum(2**i for i, tile in enumerate(layout) if tile == '#')

def find_first_repeat(initial_layout):
    seen = set()
    layout = initial_layout
    while layout not in seen:
        seen.add(layout)
        layout = evolve(layout)
    return calculate_biodiversity(layout)

initial_layout = read_input('input.txt')
result = find_first_repeat(initial_layout)
print(result)
