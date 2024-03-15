from collections import defaultdict
from dataclasses import dataclass
from typing import Set

@dataclass
class Sensor:
    pos: tuple[int, int]
    beacon: tuple[int, int]
    dist: int

def read_all(path: str) -> str:
    with open(path, 'r') as file:
        return file.read()

def manhattan(p: tuple[int, int], q: tuple[int, int]) -> int:
    return abs(p[0] - q[0]) + abs(p[1] - q[1])

def impossible(sensors: list[Sensor], y: int) -> int:
    pts: Set[int] = set()
    for s in sensors:
        dist = s.dist - abs(s.pos[1] - y)
        if dist >= 0:
            for x in range(s.pos[0] - dist, s.pos[0] + dist + 1):
                pts.add(x)
    for s in sensors:
        if s.beacon[1] == y:
            pts.discard(s.beacon[0])
    return len(pts)

def main():
    sensors = []
    input_data = read_all('input.txt')
    for line in input_data.splitlines():
        parts = line.split(': ')
        sensor_part = parts[0].split(', ')
        beacon_part = parts[1].split(', ')
        sensor_x = int(sensor_part[0].split('=')[1])
        sensor_y = int(sensor_part[1].split('=')[1])
        beacon_x = int(beacon_part[0].split('=')[1])
        beacon_y = int(beacon_part[1].split('=')[1])
        sensor = Sensor((sensor_x, sensor_y), (beacon_x, beacon_y), manhattan((sensor_x, sensor_y), (beacon_x, beacon_y)))
        sensors.append(sensor)
    print(impossible(sensors, 2000000))

if __name__ == '__main__':
    main()