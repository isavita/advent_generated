def main():
    with open("input.txt", "r") as file:
        input_data = file.read().strip()

    scanners = parse_input(input_data)

    settled = [scanners[0]]
    settled[0].absolute_coords = settled[0].relative_coords
    settled[0].fill_absolute_coords_map()

    undetermined = scanners[1:]

    while undetermined:
        for i, undet in enumerate(undetermined):
            maybe_updated, ok = find_absolute_coords_for_scanner(undet, settled)
            if ok:
                settled.append(maybe_updated)
                undetermined.pop(i)
                break

    all_beacons = set()
    for s in settled:
        all_beacons.update(s.absolute_coords_map.keys())

    print(len(all_beacons))

class Scanner:
    def __init__(self, number, coords):
        self.number = number
        self.x = 0
        self.y = 0
        self.z = 0
        self.relative_coords = coords
        self.absolute_coords = []
        self.absolute_coords_map = {}
        self.fill_rotations()

    def fill_absolute_coords_map(self):
        if not self.absolute_coords:
            raise ValueError(f"absolute coords not set for scanner {self.number}")
        self.absolute_coords_map = {tuple(ac): True for ac in self.absolute_coords}

    def fill_rotations(self):
        posX = self.relative_coords
        dir2, dir3, dir4, dir5, dir6 = [], [], [], [], []
        for x, y, z in posX:
            dir2.append((x, -y, -z))
            dir3.append((x, -z, y))
            dir4.append((-y, -z, x))
            dir5.append((-x, -z, -y))
            dir6.append((y, -z, -x))
        six_rotations = [posX, dir2, dir3, dir4, dir5, dir6]

        self.rotations = []
        for rotation in six_rotations:
            r2, r3, r4 = [], [], []
            for x, y, z in rotation:
                r2.append((-y, x, z))
                r3.append((-x, -y, z))
                r4.append((y, -x, z))
            self.rotations.extend([rotation, r2, r3, r4])

def find_absolute_coords_for_scanner(undet, settled):
    for rotated_coords in undet.rotations:
        for set_scanner in settled:
            for abs_coord in set_scanner.absolute_coords:
                for relative_coord in rotated_coords:
                    unsettled_absolute_coords = make_absolute_coords_list(abs_coord, relative_coord, rotated_coords)
                    matching_count = sum(1 for ac in unsettled_absolute_coords if tuple(ac) in set_scanner.absolute_coords_map)

                    if matching_count >= 12:
                        undet.relative_coords = rotated_coords
                        undet.absolute_coords = unsettled_absolute_coords
                        undet.fill_absolute_coords_map()
                        undet.x = abs_coord[0] - relative_coord[0]
                        undet.y = abs_coord[1] - relative_coord[1]
                        undet.z = abs_coord[2] - relative_coord[2]
                        return undet, True
    return undet, False

def make_absolute_coords_list(absolute, relative, relative_coords):
    diff = (absolute[0] - relative[0], absolute[1] - relative[1], absolute[2] - relative[2])
    return [[diff[0] + c[0], diff[1] + c[1], diff[2] + c[2]] for c in relative_coords]

def parse_input(input_data):
    scanners = []
    for raw_scanner in input_data.split("\n\n"):
        lines = raw_scanner.split("\n")
        number = int(lines[0].split()[2])
        coords = [tuple(map(int, line.split(','))) for line in lines[1:]]
        scanners.append(Scanner(number, coords))
    return scanners

if __name__ == "__main__":
    main()