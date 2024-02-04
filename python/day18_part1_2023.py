
class Coord:
    def __init__(self, x, y):
        self.X = x
        self.Y = y

    def add(self, c2):
        return Coord(self.X + c2.X, self.Y + c2.Y)

    def multiply_by_scalar(self, s):
        return Coord(self.X * s, self.Y * s)

def abs(x):
    if x < 0:
        return -x
    return x

def parse_input(input):
    north = Coord(0, -1)
    west = Coord(-1, 0)
    south = Coord(0, 1)
    east = Coord(1, 0)

    current = Coord(0, 0)
    vertices = [current]

    for line in input:
        parts = line.split(" ")
        dir_input = parts[0][0]
        length_str = parts[1]
        length = int(length_str)

        if dir_input == 'U':
            dir = north
        elif dir_input == 'L':
            dir = west
        elif dir_input == 'D':
            dir = south
        elif dir_input == 'R':
            dir = east

        current = current.add(dir.multiply_by_scalar(length))
        vertices.append(current)

    return vertices

def hex_string_to_int(hex_str):
    return int(hex_str, 16)

def shoelace(vertices):
    n = len(vertices)
    area = 0

    for i in range(n):
        next = (i + 1) % n
        area += vertices[i].X * vertices[next].Y
        area -= vertices[i].Y * vertices[next].X

    area = abs(area) // 2
    return area

def perimeter(vertices):
    n = len(vertices)
    perim = 0

    for i in range(n):
        next = (i + 1) % n
        perim += abs(vertices[i].X - vertices[next].X) + abs(vertices[i].Y - vertices[next].Y)

    return perim

def calculate_polygon_area(vertices):
    return shoelace(vertices) + perimeter(vertices) // 2 + 1

def solve(input):
    vertices = parse_input(input)
    res = calculate_polygon_area(vertices)
    return res

def read_file(file_name):
    with open(file_name, 'r') as file:
        return file.read().splitlines()

def main():
    input = read_file("input.txt")
    print(solve(input))

if __name__ == "__main__":
    main()
