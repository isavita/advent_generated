from dataclasses import dataclass
from typing import List
from os import path
import re

@dataclass
class Sensor:
    pos: tuple[int, int]
    beacon: tuple[int, int]
    dist: int

def read_all(file_path: str) -> str:
    with open(file_path, 'r') as file:
        return file.read()

def manhattan(p: tuple[int, int], q: tuple[int, int]) -> int:
    return abs(p[0] - q[0]) + abs(p[1] - q[1])

def distress(sensors: List[Sensor], max_coord: int) -> int:
    for x in range(max_coord + 1):
        y = 0
        while y <= max_coord:
            p = (x, y)
            detected = False
            skip = 0
            for s in sensors:
                if manhattan(s.pos, p) <= s.dist:
                    detected = True
                    dist = s.dist - abs(s.pos[0] - x)
                    skip = max(skip, dist + s.pos[1] - y)
                    if skip > 0:
                        y += skip
                        break
            if not detected:
                return x * 4000000 + y
            y += 1
    return -1

def main():
    sensors = []
    input_text = read_all('input.txt')
    for line in input_text.strip().split('\n'):
        parts = re.findall(r'-?\d+', line)
        sensor_pos = (int(parts[0]), int(parts[1]))
        beacon_pos = (int(parts[2]), int(parts[3]))
        sensors.append(Sensor(sensor_pos, beacon_pos, manhattan(sensor_pos, beacon_pos)))
    print(distress(sensors, 4000000))

if __name__ == '__main__':
    main()