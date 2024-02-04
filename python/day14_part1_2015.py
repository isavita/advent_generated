
from collections import namedtuple

Reindeer = namedtuple('Reindeer', 'name speed fly_time rest_time')
reindeers = []

with open('input.txt', 'r') as file:
    for line in file:
        data = line.strip().split()
        name = data[0]
        speed = int(data[3])
        fly_time = int(data[6])
        rest_time = int(data[13])
        reindeers.append(Reindeer(name, speed, fly_time, rest_time))

def distance_traveled(reindeer, time):
    cycle_time = reindeer.fly_time + reindeer.rest_time
    full_cycles = time // cycle_time
    remaining_time = time % cycle_time
    distance = full_cycles * reindeer.speed * reindeer.fly_time
    distance += min(remaining_time, reindeer.fly_time) * reindeer.speed
    return distance

time = 2503
print(max(distance_traveled(r, time) for r in reindeers))
