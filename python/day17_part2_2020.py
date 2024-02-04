
def simulate_cycle_4d(active_cubes):
    new_active_cubes = {}
    neighbor_counts = {}

    for coord in active_cubes:
        for dw in range(-1, 2):
            for dz in range(-1, 2):
                for dy in range(-1, 2):
                    for dx in range(-1, 2):
                        if dw == 0 and dz == 0 and dy == 0 and dx == 0:
                            continue
                        neighbor = (coord[0] + dx, coord[1] + dy, coord[2] + dz, coord[3] + dw)
                        neighbor_counts[neighbor] = neighbor_counts.get(neighbor, 0) + 1

    for coord, count in neighbor_counts.items():
        if count == 3 or (count == 2 and coord in active_cubes):
            new_active_cubes[coord] = True

    return new_active_cubes

def main():
    with open("input.txt", "r") as file:
        initialState = file.read().strip().split("\n")

    active_cubes = {}

    for y, line in enumerate(initialState):
        for x, char in enumerate(line):
            if char == '#':
                active_cubes[(x, y, 0, 0)] = True

    for cycle in range(6):
        active_cubes = simulate_cycle_4d(active_cubes)

    print(len(active_cubes))

if __name__ == "__main__":
    main()
