import math

class Coord:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

class Point:
    def __init__(self, pos, vel):
        self.pos = pos
        self.vel = vel

def parse_input(lines):
    points = []
    for line in lines:
        parts = line.split(" @ ")
        pos_parts = parts[0].split(", ")
        vel_parts = parts[1].split(", ")
        pos_x, pos_y, pos_z = map(int, pos_parts)
        vel_x, vel_y, vel_z = map(int, vel_parts)
        pos = Coord(pos_x, pos_y, pos_z)
        vel = Coord(vel_x, vel_y, vel_z)
        points.append(Point(pos, vel))
    return points

def is_intersecting_2d(p1, p2):
    det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
    if det == 0:
        return False, Coord(0, 0, 0), 0, 0
    t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
    t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
    coord = Coord(p1.pos.x + p1.vel.x * t1, p1.pos.y + p1.vel.y * t1, 0)
    return True, coord, t1, t2

def solve(lines, min_val, max_val):
    points = parse_input(lines)
    count = 0
    for i in range(len(points)):
        for j in range(i):
            is_intersecting, coord, time1, time2 = is_intersecting_2d(points[i], points[j])
            is_in_bound = min_val <= coord.x <= max_val and min_val <= coord.y <= max_val
            if is_intersecting and is_in_bound and time1 >= 0 and time2 >= 0:
                count += 1
    return count

with open("input.txt", "r") as file:
    lines = [line.strip() for line in file.readlines()]
    print(solve(lines, 200000000000000, 400000000000000))