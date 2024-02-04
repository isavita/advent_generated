
import hashlib

class Point:
    def __init__(self, x, y, path):
        self.x = x
        self.y = y
        self.path = path

def read_passcode(filename):
    with open(filename, 'r') as file:
        return file.readline().strip()

def find_longest_path_length(passcode):
    longest = 0
    queue = [Point(0, 0, "")]
    while queue:
        point = queue.pop(0)

        if point.x == 3 and point.y == 3:
            if len(point.path) > longest:
                longest = len(point.path)
            continue

        for dir in get_open_doors(passcode, point.path):
            next_point = Point(point.x, point.y, point.path + dir)
            if dir == "U":
                next_point.y -= 1
            elif dir == "D":
                next_point.y += 1
            elif dir == "L":
                next_point.x -= 1
            elif dir == "R":
                next_point.x += 1

            if 0 <= next_point.x < 4 and 0 <= next_point.y < 4:
                queue.append(next_point)

    return longest

def get_open_doors(passcode, path):
    hash_input = passcode + path
    hash = hashlib.md5(hash_input.encode()).hexdigest()
    doors = []
    if 'b' <= hash[0] <= 'f':
        doors.append("U")
    if 'b' <= hash[1] <= 'f':
        doors.append("D")
    if 'b' <= hash[2] <= 'f':
        doors.append("L")
    if 'b' <= hash[3] <= 'f':
        doors.append("R")
    return doors

passcode = read_passcode("input.txt")
longest_path_length = find_longest_path_length(passcode)
print(longest_path_length)
