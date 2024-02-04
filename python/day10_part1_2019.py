
import math

def get_angle(a, b):
    return math.atan2(b[1] - a[1], b[0] - a[0])

def get_distance(a, b):
    return math.sqrt((b[0] - a[0])**2 + (b[1] - a[1])**2)

def count_visible_asteroids(station, asteroids):
    angles = set()
    for asteroid in asteroids:
        if asteroid != station:
            angle = get_angle(station, asteroid)
            angles.add(angle)
    return len(angles)

def find_best_location(asteroids):
    max_visible = 0
    best_location = None
    for station in asteroids:
        visible = count_visible_asteroids(station, asteroids)
        if visible > max_visible:
            max_visible = visible
            best_location = station
    return max_visible

def main():
    with open('input.txt', 'r') as file:
        lines = file.read().splitlines()
    
    asteroids = []
    for y in range(len(lines)):
        for x in range(len(lines[y])):
            if lines[y][x] == '#':
                asteroids.append((x, y))
    
    result = find_best_location(asteroids)
    print(result)

if __name__ == "__main__":
    main()
