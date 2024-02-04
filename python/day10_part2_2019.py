
import math

class Asteroid:
    def __init__(self, x, y, angle=0.0, dist=0.0):
        self.x = x
        self.y = y
        self.angle = angle
        self.dist = dist

def read_asteroids(filename):
    asteroids = []
    with open(filename, 'r') as file:
        y = 0
        for line in file:
            row = []
            for x, char in enumerate(line.strip()):
                row.append(char == '#')
            asteroids.append(row)
            y += 1
    return asteroids

def vaporize_asteroids(asteroids, station):
    targets = []
    for y, row in enumerate(asteroids):
        for x, is_asteroid in enumerate(row):
            if is_asteroid and not (x == station[0] and y == station[1]):
                angle = math.atan2(y - station[1], x - station[0])
                dist = math.hypot(x - station[0], y - station[1])
                if angle < -math.pi / 2:
                    angle += 2 * math.pi
                targets.append(Asteroid(x, y, angle, dist))
    targets.sort(key=lambda a: (a.angle, a.dist))
    vaporized = []
    while targets:
        last_angle = -math.inf
        i = 0
        while i < len(targets):
            if targets[i].angle != last_angle:
                vaporized.append(targets.pop(i))
                last_angle = vaporized[-1].angle
            else:
                i += 1
    return vaporized

def find_best_asteroid_location(asteroids):
    best_location = (0, 0)
    max_count = 0
    for y, row in enumerate(asteroids):
        for x, is_asteroid in enumerate(row):
            if is_asteroid:
                count = count_visible_asteroids(asteroids, x, y)
                if count > max_count:
                    max_count = count
                    best_location = (x, y)
    return best_location, max_count

def count_visible_asteroids(asteroids, x, y):
    angles = set()
    for other_y, row in enumerate(asteroids):
        for other_x, is_asteroid in enumerate(row):
            if is_asteroid and not (other_x == x and other_y == y):
                angle = math.atan2(other_y - y, other_x - x)
                angles.add(angle)
    return len(angles)

if __name__ == "__main__":
    asteroids = read_asteroids("input.txt")
    station, _ = find_best_asteroid_location(asteroids)
    vaporized = vaporize_asteroids(asteroids, station)
    if len(vaporized) >= 200:
        result = vaporized[199].x * 100 + vaporized[199].y
        print(result)
    else:
        print("Less than 200 asteroids were vaporized.")
