from collections import namedtuple

Cuboid = namedtuple('Cuboid', ['x1', 'x2', 'y1', 'y2', 'z1', 'z2', 'on'])

def parse_input(line):
    state, coords = line.split()
    x, y, z = [map(int, c[2:].split('..')) for c in coords.split(',')]
    return Cuboid(x[0], x[1], y[0], y[1], z[0], z[1], state == 'on')

def intersect(c1, c2):
    return (max(c1.x1, c2.x1), min(c1.x2, c2.x2),
            max(c1.y1, c2.y1), min(c1.y2, c2.y2),
            max(c1.z1, c2.z1), min(c1.z2, c2.z2))

def volume(c):
    return max(0, c.x2 - c.x1 + 1) * max(0, c.y2 - c.y1 + 1) * max(0, c.z2 - c.z1 + 1)

def subtract(c1, c2):
    ix, iy, iz = intersect(c1, c2)
    if ix[1] < ix[0] or iy[1] < iy[0] or iz[1] < iz[0]:
        return [c1]
    result = []
    if c1.x1 < ix[0]:
        result.append(Cuboid(c1.x1, ix[0]-1, c1.y1, c1.y2, c1.z1, c1.z2, c1.on))
    if ix[1] < c1.x2:
        result.append(Cuboid(ix[1]+1, c1.x2, c1.y1, c1.y2, c1.z1, c1.z2, c1.on))
    if c1.y1 < iy[0]:
        result.append(Cuboid(ix[0], ix[1], c1.y1, iy[0]-1, c1.z1, c1.z2, c1.on))
    if iy[1] < c1.y2:
        result.append(Cuboid(ix[0], ix[1], iy[1]+1, c1.y2, c1.z1, c1.z2, c1.on))
    if c1.z1 < iz[0]:
        result.append(Cuboid(ix[0], ix[1], iy[0], iy[1], c1.z1, iz[0]-1, c1.on))
    if iz[1] < c1.z2:
        result.append(Cuboid(ix[0], ix[1], iy[0], iy[1], iz[1]+1, c1.z2, c1.on))
    return result

def process_steps(steps, part1=True):
    cuboids = []
    for step in steps:
        if part1 and (abs(step.x1) > 50 or abs(step.x2) > 50 or
                      abs(step.y1) > 50 or abs(step.y2) > 50 or
                      abs(step.z1) > 50 or abs(step.z2) > 50):
            continue
        new_cuboids = []
        for c in cuboids:
            new_cuboids.extend(subtract(c, step))
        if step.on:
            new_cuboids.append(step)
        cuboids = new_cuboids
    return sum(volume(c) for c in cuboids)

def solve(input_data):
    steps = [parse_input(line) for line in input_data.strip().split('\n')]
    part1 = process_steps(steps, part1=True)
    part2 = process_steps(steps, part1=False)
    return part1, part2

# Example usage:
input_data = """
on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
"""

part1, part2 = solve(input_data)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
