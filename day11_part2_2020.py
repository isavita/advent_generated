
from copy import deepcopy

def count_occupied_seats(seats):
    rows = len(seats)
    cols = len(seats[0])
    occupied = 0
    for i in range(rows):
        for j in range(cols):
            if seats[i][j] == '#':
                occupied += 1
    return occupied

def check_adjacent(seats, i, j, rows, cols):
    count = 0
    for x in range(max(i-1, 0), min(i+2, rows)):
        for y in range(max(j-1, 0), min(j+2, cols)):
            if x == i and y == j:
                continue
            if seats[x][y] == '#':
                count += 1
    return count

def check_direction(seats, i, j, dx, dy, rows, cols):
    x = i + dx
    y = j + dy
    while 0 <= x < rows and 0 <= y < cols:
        if seats[x][y] == '#':
            return 1
        if seats[x][y] == 'L':
            return 0
        x += dx
        y += dy
    return 0

def check_visible(seats, i, j, rows, cols):
    count = 0
    directions = [(dx, dy) for dx in [-1, 0, 1] for dy in [-1, 0, 1] if dx != 0 or dy != 0]
    for dx, dy in directions:
        count += check_direction(seats, i, j, dx, dy, rows, cols)
    return count

def simulate_seating(seats, rows, cols, tolerance, visibility):
    while True:
        new_seats = deepcopy(seats)
        changes = 0
        for i in range(rows):
            for j in range(cols):
                if seats[i][j] == 'L' and (visibility == 1 and check_visible(seats, i, j, rows, cols) == 0 or visibility == 0 and check_adjacent(seats, i, j, rows, cols) == 0):
                    new_seats[i][j] = '#'
                    changes += 1
                elif seats[i][j] == '#' and (visibility == 1 and check_visible(seats, i, j, rows, cols) >= tolerance or visibility == 0 and check_adjacent(seats, i, j, rows, cols) >= tolerance):
                    new_seats[i][j] = 'L'
                    changes += 1
        if changes == 0:
            return count_occupied_seats(seats)
        seats = deepcopy(new_seats)

with open('input.txt', 'r') as file:
    seats = [list(line.strip()) for line in file.readlines()]
    rows = len(seats)
    cols = len(seats[0])

    print(simulate_seating(seats, rows, cols, 4, 0))  # Part One
    print(simulate_seating(seats, rows, cols, 5, 1))  # Part Two
