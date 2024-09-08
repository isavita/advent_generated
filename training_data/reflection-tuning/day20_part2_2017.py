import re
from collections import defaultdict

def parse_input(filename):
    particles = []
    with open(filename, 'r') as f:
        for line in f:
            nums = list(map(int, re.findall(r'-?\d+', line)))
            particles.append((tuple(nums[0:3]), tuple(nums[3:6]), tuple(nums[6:9])))
    return particles

def manhattan_distance(point):
    return sum(abs(x) for x in point)

def vector_add(v1, v2):
    return tuple(a + b for a, b in zip(v1, v2))

def part1(particles):
    return min(range(len(particles)), key=lambda i: (
        manhattan_distance(particles[i][2]),
        manhattan_distance(particles[i][1]),
        manhattan_distance(particles[i][0])
    ))

def part2(particles):
    active_particles = set(range(len(particles)))
    no_collisions_count = 0

    while no_collisions_count < 100:  # Arbitrary large number of steps
        positions = defaultdict(list)
        for i in active_particles:
            p, v, a = particles[i]
            v = vector_add(v, a)
            p = vector_add(p, v)
            particles[i] = (p, v, a)
            positions[p].append(i)

        collided = set()
        for pos, particle_list in positions.items():
            if len(particle_list) > 1:
                collided.update(particle_list)

        if collided:
            active_particles -= collided
            no_collisions_count = 0
        else:
            no_collisions_count += 1

    return len(active_particles)

particles = parse_input("input.txt")
print("Part 1:", part1(particles))
print("Part 2:", part2(particles))
