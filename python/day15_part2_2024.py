
def solve(input_str):
    blocks = input_str.strip().split("\n\n")
    lines = blocks[0].split("\n")
    m = {}
    for y, row in enumerate(lines):
        for x, char in enumerate(row):
            m[complex(x, y)] = char
    steps = []
    for char in blocks[1].replace("\n", ""):
        if char == '^':
            steps.append(complex(0, -1))
        elif char == '<':
            steps.append(complex(-1, 0))
        elif char == '>':
            steps.append(complex(1, 0))
        elif char == 'v':
            steps.append(complex(0, 1))
    robot = next(k for k, v in m.items() if v == '@')
    for dir in steps:
        if try_to_step(m, robot, dir):
            robot += dir
    return sum(k.real + 100 * k.imag for k, v in m.items() if v in ['[', 'O'])

def try_to_step(m, pos, dir):
    orig = m.copy()
    if m.get(pos) == '.':
        return True
    elif m.get(pos) in ['O', '@']:
        if try_to_step(m, pos + dir, dir):
            m[pos + dir] = m[pos]
            m[pos] = '.'
            return True
    elif m.get(pos) == ']':
        if try_to_step(m, pos + complex(-1, 0), dir):
            return True
    elif m.get(pos) == '[':
        if dir == complex(-1, 0):
            if try_to_step(m, pos + complex(-1, 0), dir):
                m[pos + complex(-1, 0)] = '['
                m[pos] = ']'
                m[pos + complex(1, 0)] = '.'
                return True
        elif dir == complex(1, 0):
            if try_to_step(m, pos + 2 * complex(1, 0), dir):
                m[pos] = '.'
                m[pos + complex(1, 0)] = '['
                m[pos + 2 * complex(1, 0)] = ']'
                return True
        else:
            if try_to_step(m, pos + dir, dir) and try_to_step(m, pos + complex(1, 0) + dir, dir):
                m[pos] = '.'
                m[pos + complex(1, 0)] = '.'
                m[pos + dir] = '['
                m[pos + dir + complex(1, 0)] = ']'
                return True
    m.clear()
    m.update(orig)
    return False

def scale_up(input_str):
    s = input_str.replace("#", "##")
    s = s.replace(".", "..")
    s = s.replace("O", "[]")
    s = s.replace("@", "@.")
    return s

with open("input.txt", "r") as f:
    input_str = f.read()
print(int(solve(input_str)))
print(int(solve(scale_up(input_str))))
