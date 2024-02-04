
class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def add(self, other):
        return Coord(self.x + other.x, self.y + other.y)

    def multiply_by_scalar(self, s):
        return Coord(self.x * s, self.y * s)

def abs_val(x):
    if x < 0:
        return -x
    return x

def parse_input(input_lines):
    north = Coord(0, -1)
    west = Coord(-1, 0)
    south = Coord(0, 1)
    east = Coord(1, 0)

    current = Coord(0, 0)
    vertices = [current]

    for line in input_lines:
        parts = line.split(" ")
        color = parts[2]
        dir_input = color[7]
        length_str = color[2:7]
        length = int(length_str, 16)

        if dir_input == '3':
            direction = north
        elif dir_input == '2':
            direction = west
        elif dir_input == '1':
            direction = south
        elif dir_input == '0':
            direction = east

        current = current.add(direction.multiply_by_scalar(length))
        vertices.append(current)

    return vertices

def shoelace(vertices):
    n = len(vertices)
    area = 0

    for i in range(n):
        next_idx = (i + 1) % n
        area += vertices[i].x * vertices[next_idx].y
        area -= vertices[i].y * vertices[next_idx].x

    area = abs_val(area) // 2
    return area

def perimeter(vertices):
    n = len(vertices)
    perim = 0

    for i in range(n):
        next_idx = (i + 1) % n
        perim += abs_val(vertices[i].x - vertices[next_idx].x) + abs_val(vertices[i].y - vertices[next_idx].y)

    return perim

def calculate_polygon_area(vertices):
    return shoelace(vertices) + perimeter(vertices) // 2 + 1

def solve(input_lines):
    vertices = parse_input(input_lines)
    res = calculate_polygon_area(vertices)
    return res

def read_file(file_name):
    with open(file_name, 'r') as file:
        return file.read().strip().split('\n')

input_lines = read_file("input.txt")
print(solve(input_lines))
