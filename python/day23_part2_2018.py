import re
import heapq
from typing import List, Tuple

def parse_input(file_path: str) -> List[Tuple[int, int, int, int]]:
    """
    Parses the input file and returns a list of nanobots.
    Each nanobot is represented as a tuple (x, y, z, r).
    """
    nanobots = []
    pattern = re.compile(r"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)")
    with open(file_path, 'r') as file:
        for line in file:
            match = pattern.match(line.strip())
            if match:
                x, y, z, r = map(int, match.groups())
                nanobots.append((x, y, z, r))
    return nanobots

def manhattan_distance(a: Tuple[int, int, int], b: Tuple[int, int, int]) -> int:
    """
    Calculates the Manhattan distance between two points a and b.
    """
    return abs(a[0] - b[0]) + abs(a[1] - b[1]) + abs(a[2] - b[2])

def part_one(nanobots: List[Tuple[int, int, int, int]]) -> int:
    """
    Finds the number of nanobots in range of the nanobot with the largest signal radius.
    """
    # Find the nanobot with the largest signal radius
    strongest = max(nanobots, key=lambda bot: bot[3])
    sx, sy, sz, sr = strongest
    count = 0
    for bot in nanobots:
        distance = manhattan_distance((sx, sy, sz), (bot[0], bot[1], bot[2]))
        if distance <= sr:
            count += 1
    return count

def min_distance_to_origin(x: int, y: int, z: int, size: int) -> int:
    """
    Calculates the minimum Manhattan distance from any point in the cube to the origin.
    The cube is defined by its lower corner (x, y, z) and its size.
    """
    dx = 0
    if x > 0:
        dx = x
    elif x + size - 1 < 0:
        dx = -(x + size - 1)
    # else, 0 is within the cube along the x-axis

    dy = 0
    if y > 0:
        dy = y
    elif y + size - 1 < 0:
        dy = -(y + size - 1)
    # else, 0 is within the cube along the y-axis

    dz = 0
    if z > 0:
        dz = z
    elif z + size - 1 < 0:
        dz = -(z + size - 1)
    # else, 0 is within the cube along the z-axis

    return dx + dy + dz

def part_two(nanobots: List[Tuple[int, int, int, int]]) -> int:
    """
    Finds the smallest Manhattan distance to the origin for a point that is in range of the most nanobots.
    """
    # Determine the bounds
    min_x = min(bot[0] for bot in nanobots)
    max_x = max(bot[0] for bot in nanobots)
    min_y = min(bot[1] for bot in nanobots)
    max_y = max(bot[1] for bot in nanobots)
    min_z = min(bot[2] for bot in nanobots)
    max_z = max(bot[2] for bot in nanobots)

    # Determine the initial cube size as a power of 2
    size = 1
    while size < max(max_x - min_x, max_y - min_y, max_z - min_z):
        size *= 2

    # Initialize the heap with the initial cube
    # Heap elements are tuples: (-count, distance, size, x, y, z)
    heap = []
    heapq.heappush(heap, (
        0,  # Negative count for max heap
        min_distance_to_origin(min_x, min_y, min_z, size),
        size,
        min_x,
        min_y,
        min_z
    ))

    best_distance = None
    best_count = -1

    while heap:
        neg_count, distance, size, x, y, z = heapq.heappop(heap)
        count = -neg_count

        if size == 1:
            # This is a single point
            if count > best_count or (count == best_count and distance < best_distance):
                best_count = count
                best_distance = distance
                # Since heap is ordered by count descending and distance ascending,
                # the first point found with the highest count and smallest distance is optimal
                break
            continue

        # Split the cube into smaller cubes
        half = size // 2
        for dx in [0, half]:
            for dy in [0, half]:
                for dz in [0, half]:
                    nx = x + dx
                    ny = y + dy
                    nz = z + dz
                    new_size = half
                    if new_size < 1:
                        new_size = 1

                    # Count how many nanobots are in range of this sub-cube
                    count = 0
                    for bot in nanobots:
                        bx, by, bz, br = bot
                        # Calculate the minimal Manhattan distance from the nanobot to the cube
                        d = 0
                        if bx < nx:
                            d += nx - bx
                        elif bx > nx + new_size - 1:
                            d += bx - (nx + new_size -1)
                        if by < ny:
                            d += ny - by
                        elif by > ny + new_size -1:
                            d += by - (ny + new_size -1)
                        if bz < nz:
                            d += nz - bz
                        elif bz > nz + new_size -1:
                            d += bz - (nz + new_size -1)
                        if d <= br:
                            count += 1

                    # Calculate the distance to origin from the cube
                    distance = min_distance_to_origin(nx, ny, nz, new_size)

                    heapq.heappush(heap, (
                        -count,       # Negative count for max heap
                        distance,     # Distance to origin
                        new_size,     # Size of the cube
                        nx,
                        ny,
                        nz
                    ))

    return best_distance

def main():
    input_file = 'input.txt'
    nanobots = parse_input(input_file)

    # Part One
    count_in_range = part_one(nanobots)
    print(f"Part One: {count_in_range}")

    # Part Two
    shortest_distance = part_two(nanobots)
    print(f"Part Two: {shortest_distance}")

if __name__ == "__main__":
    main()
