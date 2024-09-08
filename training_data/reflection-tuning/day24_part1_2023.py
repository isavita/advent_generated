import numpy as np
from itertools import combinations

def parse_input(filename):
    hailstones = []
    with open(filename, 'r') as file:
        for line in file:
            pos, vel = line.strip().split('@')
            pos = np.array([int(x) for x in pos.split(',')])
            vel = np.array([int(x) for x in vel.split(',')])
            hailstones.append((pos[:2], vel[:2]))  # Only consider x and y
    return hailstones

def intersect(h1, h2, test_area):
    p1, v1 = h1
    p2, v2 = h2
    
    # Check if paths are parallel
    if np.cross(v1, v2) == 0:
        return False
    
    # Solve the linear system to find intersection time
    A = np.column_stack((v1, -v2))
    b = p2 - p1
    t = np.linalg.solve(A, b)
    
    # Check if intersection is in the future for both hailstones
    if t[0] < 0 or t[1] < 0:
        return False
    
    # Calculate intersection point
    intersection = p1 + t[0] * v1
    
    # Check if intersection is within test area
    return all(test_area[0] <= coord <= test_area[1] for coord in intersection)

def count_intersections(hailstones, test_area):
    count = 0
    for h1, h2 in combinations(hailstones, 2):
        if intersect(h1, h2, test_area):
            count += 1
    return count

# Main execution
hailstones = parse_input("input.txt")
test_area = (200000000000000, 400000000000000)
result = count_intersections(hailstones, test_area)
print(result)
