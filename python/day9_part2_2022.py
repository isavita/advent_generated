
def read_all():
    with open("input.txt", "r") as file:
        return file.read()

def visited(input, ropelen):
    rope = [(0, 0)] * ropelen
    visited = set()
    for line in input.split("\n"):
        parts = line.split()
        if len(parts) != 2: continue
        b, n = parts[0], int(parts[1])
        d = dir_from_byte(b)
        for _ in range(n):
            rope[0] = tuple(map(sum, zip(rope[0], d)))
            for j in range(1, ropelen):
                rope[j] = next_point(rope[j-1], rope[j])
            visited.add(rope[-1])
    return len(visited)

def next_point(head, tail):
    if abs(head[0] - tail[0]) <= 1 and abs(head[1] - tail[1]) <= 1:
        return tail
    return (tail[0] + sign(head[0] - tail[0]), tail[1] + sign(head[1] - tail[1]))

def abs(x):
    return -x if x < 0 else x

def sign(n):
    return (n > 0) - (n < 0)

dir_map = {
    'N': (0, 1),
    'E': (1, 0),
    'S': (0, -1),
    'W': (-1, 0),
    'U': (0, 1),
    'R': (1, 0),
    'D': (0, -1),
    'L': (-1, 0),
    '^': (0, 1),
    '>': (1, 0),
    'v': (0, -1),
    '<': (-1, 0),
}

def dir_from_byte(b):
    return dir_map[b]

if __name__ == "__main__":
    input = read_all()
    print(visited(input, 10))
