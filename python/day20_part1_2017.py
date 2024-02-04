
import math

def abs(x):
    return -x if x < 0 else x

def manhattan(x):
    return abs(x[0]) + abs(x[1]) + abs(x[2])

particles = []

with open("input.txt", "r") as file:
    for line in file:
        parts = line.strip().split(", ")

        p = [0, 0, 0]
        v = [0, 0, 0]
        a = [0, 0, 0]

        for i, part in enumerate(parts):
            coords = part[3:-1].split(",")
            for j, coord in enumerate(coords):
                num = int(coord)
                if i == 0:
                    p[j] = num
                elif i == 1:
                    v[j] = num
                elif i == 2:
                    a[j] = num

        particles.append((p, v, a))

closestParticle = 0
minAccel = math.inf
minVelocity = math.inf
minPosition = math.inf

for i, particle in enumerate(particles):
    accel = manhattan(particle[2])
    velocity = manhattan(particle[1])
    position = manhattan(particle[0])

    if accel < minAccel or (accel == minAccel and velocity < minVelocity) or (accel == minAccel and velocity == minVelocity and position < minPosition):
        minAccel = accel
        minVelocity = velocity
        minPosition = position
        closestParticle = i

print(closestParticle)
