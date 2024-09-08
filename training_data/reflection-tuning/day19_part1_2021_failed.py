from itertools import permutations, product

def rotate_point(point, rotation):
    return tuple(point[abs(i) - 1] * int(i / abs(i)) for i in rotation)

def all_rotations(point):
    rotations = list(permutations([1, 2, 3])) + list(permutations([-1, -2, -3]))
    return [rotate_point(point, rotation) for rotation in rotations]

def find_overlap(beacons1, beacons2):
    for rotation in product([-1, 1], repeat=3):
        rotated_beacons2 = [rotate_point(b, rotation) for b in beacons2]
        for b1 in beacons1:
            for b2 in rotated_beacons2:
                offset = tuple(b1[i] - b2[i] for i in range(3))
                aligned_beacons2 = set(tuple(b[i] + offset[i] for i in range(3)) for b in rotated_beacons2)
                if len(beacons1.intersection(aligned_beacons2)) >= 12:
                    return offset, aligned_beacons2
    return None, None

def main():
    scanners = []
    current_scanner = []
    
    with open('input.txt', 'r') as file:
        for line in file:
            line = line.strip()
            if line.startswith('---'):
                if current_scanner:
                    scanners.append(set(current_scanner))
                    current_scanner = []
            elif line:
                current_scanner.append(tuple(map(int, line.split(','))))
    
    if current_scanner:
        scanners.append(set(current_scanner))

    all_beacons = scanners[0]
    remaining_scanners = scanners[1:]
    scanner_positions = [(0, 0, 0)]

    while remaining_scanners:
        for i, scanner in enumerate(remaining_scanners):
            offset, aligned_beacons = find_overlap(all_beacons, scanner)
            if offset:
                all_beacons.update(aligned_beacons)
                scanner_positions.append(offset)
                remaining_scanners.pop(i)
                break
        else:
            print("Error: Could not align all scanners")
            return

    print(f"Total number of beacons: {len(all_beacons)}")

if __name__ == "__main__":
    main()
