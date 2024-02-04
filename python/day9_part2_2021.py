
import sys

def is_low_point(heightmap, x, y):
    height = heightmap[y][x]
    if x > 0 and heightmap[y][x-1] <= height:
        return False
    if x < len(heightmap[y])-1 and heightmap[y][x+1] <= height:
        return False
    if y > 0 and heightmap[y-1][x] <= height:
        return False
    if y < len(heightmap)-1 and heightmap[y+1][x] <= height:
        return False
    return True

def explore_basin(heightmap, x, y, visited):
    if visited.get((x, y)) or heightmap[y][x] == 9:
        return 0
    visited[(x, y)] = True
    size = 1

    directions = [(0, -1), (-1, 0), (0, 1), (1, 0)]
    for dir in directions:
        new_x, new_y = x+dir[0], y+dir[1]
        if new_x >= 0 and new_x < len(heightmap[0]) and new_y >= 0 and new_y < len(heightmap):
            size += explore_basin(heightmap, new_x, new_y, visited)
    return size

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        heightmap = []
        for line in file:
            row = [int(char) for char in line.strip()]
            heightmap.append(row)

    basin_sizes = []
    visited = {}

    for y, row in enumerate(heightmap):
        for x in range(len(row)):
            if is_low_point(heightmap, x, y):
                size = explore_basin(heightmap, x, y, visited)
                basin_sizes.append(size)

    basin_sizes.sort(reverse=True)
    result = basin_sizes[0] * basin_sizes[1] * basin_sizes[2]
    print(result)
