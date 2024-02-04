
with open("input.txt", "r") as file:
    cubes = {}
    for line in file:
        coords = line.strip().split(",")
        x, y, z = map(int, coords)
        cubes[(x, y, z)] = True

def calculateExposedSides(p, cubes):
    directions = [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]
    exposedSides = 6
    for dir in directions:
        adjacent = (p[0] + dir[0], p[1] + dir[1], p[2] + dir[2])
        if adjacent in cubes:
            exposedSides -= 1
    return exposedSides

surfaceArea = 0
for cube in cubes:
    surfaceArea += calculateExposedSides(cube, cubes)

print(surfaceArea)
