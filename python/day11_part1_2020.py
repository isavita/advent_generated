
from copy import deepcopy

def count_occupied_seats(seating_area):
    new_seating_area = deepcopy(seating_area)
    rows, cols = len(seating_area), len(seating_area[0])
    directions = [(i, j) for i in range(-1, 2) for j in range(-1, 2) if not (i == 0 and j == 0)]

    while True:
        changes = 0
        for i in range(rows):
            for j in range(cols):
                if seating_area[i][j] == '.':
                    continue

                occupied_neighbors = 0
                for dx, dy in directions:
                    x, y = i + dx, j + dy
                    if 0 <= x < rows and 0 <= y < cols and seating_area[x][y] == '#':
                        occupied_neighbors += 1

                if seating_area[i][j] == 'L' and occupied_neighbors == 0:
                    new_seating_area[i][j] = '#'
                    changes += 1
                elif seating_area[i][j] == '#' and occupied_neighbors >= 4:
                    new_seating_area[i][j] = 'L'
                    changes += 1

        if changes == 0:
            break

        seating_area = deepcopy(new_seating_area)

    return sum(row.count('#') for row in seating_area)

with open('input.txt', 'r') as file:
    seating_area = [list(line.strip()) for line in file]

print(count_occupied_seats(seating_area))
