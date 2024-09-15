import sys
import math
from itertools import combinations
from collections import defaultdict

def read_input(filename):
    scanners = []
    with open(filename, 'r') as f:
        scanner = []
        for line in f:
            line = line.strip()
            if line.startswith('---'):
                if scanner:
                    scanners.append(scanner)
                    scanner = []
            elif line:
                scanner.append(tuple(map(int, line.split(','))))
        if scanner:
            scanners.append(scanner)
    return scanners

def get_rotations():
    # 24 rotation functions
    rotations = []
    # Define all 24 possible orientations
    axes = [
        (1, 0, 0),
        (-1, 0, 0),
        (0, 1, 0),
        (0, -1, 0),
        (0, 0, 1),
        (0, 0, -1),
    ]
    # Generate all unique rotation matrices
    from itertools import product
    def rotate(p, rot):
        return (
            p[0]*rot[0][0] + p[1]*rot[0][1] + p[2]*rot[0][2],
            p[0]*rot[1][0] + p[1]*rot[1][1] + p[2]*rot[1][2],
            p[0]*rot[2][0] + p[1]*rot[2][1] + p[2]*rot[2][2],
        )
    # Precompute all 24 rotation matrices
    rotation_matrices = []
    # Basis vectors
    basis = [
        (1,0,0),
        (0,1,0),
        (0,0,1)
    ]
    # Generate all rotation matrices using axis permutations and sign changes
    from itertools import permutations, product
    for perm in permutations([0,1,2]):
        for signs in product([1,-1], repeat=3):
            rot = [
                [0,0,0],
                [0,0,0],
                [0,0,0],
            ]
            for i in range(3):
                rot[i][perm[i]] = signs[i]
            # Check if determinant is 1 to ensure proper rotation (no reflection)
            det = (
                rot[0][0]*(rot[1][1]*rot[2][2] - rot[1][2]*rot[2][1]) -
                rot[0][1]*(rot[1][0]*rot[2][2] - rot[1][2]*rot[2][0]) +
                rot[0][2]*(rot[1][0]*rot[2][1] - rot[1][1]*rot[2][0])
            )
            if det == 1:
                rotation_matrices.append(rot)
    # Now, define rotation functions
    for rot in rotation_matrices:
        rotations.append(lambda p, rot=rot: (
            p[0]*rot[0][0] + p[1]*rot[0][1] + p[2]*rot[0][2],
            p[0]*rot[1][0] + p[1]*rot[1][1] + p[2]*rot[1][2],
            p[0]*rot[2][0] + p[1]*rot[2][1] + p[2]*rot[2][2],
        ))
    return rotations

def add(p1, p2):
    return tuple(a + b for a, b in zip(p1, p2))

def subtract(p1, p2):
    return tuple(a - b for a, b in zip(p1, p2))

def manhattan(p1, p2):
    return sum(abs(a - b) for a, b in zip(p1, p2))

def solve(scanners):
    rotations = get_rotations()
    aligned = {0}
    scanner_positions = {0: (0,0,0)}
    beacons = set(scanners[0])
    pending = set(range(1, len(scanners)))
    while pending:
        found = False
        for scanner in list(pending):
            for rot in rotations:
                rotated = [rot(p) for p in scanners[scanner]]
                deltas = defaultdict(int)
                # Count how many times a particular delta occurs
                for beacon in rotated:
                    for aligned_beacon in beacons:
                        delta = subtract(aligned_beacon, beacon)
                        deltas[delta] += 1
                # Find the delta with the maximum count
                max_delta, count = max(deltas.items(), key=lambda x: x[1])
                if count >= 12:
                    # Align this scanner
                    scanner_positions[scanner] = max_delta
                    # Add all beacons with the rotation and translation
                    for beacon in rotated:
                        beacons.add(add(beacon, max_delta))
                    aligned.add(scanner)
                    pending.remove(scanner)
                    found = True
                    break
            if found:
                break
        if not found:
            print("No alignment found for remaining scanners.")
            break
    # Now compute the largest Manhattan distance between any two scanners
    max_distance = 0
    positions = list(scanner_positions.values())
    for i in range(len(positions)):
        for j in range(i+1, len(positions)):
            dist = manhattan(positions[i], positions[j])
            if dist > max_distance:
                max_distance = dist
    return max_distance

def main():
    scanners = read_input('input.txt')
    result = solve(scanners)
    print(result)

if __name__ == '__main__':
    main()
