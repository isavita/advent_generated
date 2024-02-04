
import os
import sys
import string

def read_input_file(file_path):
    matrix = []
    with open(file_path, 'r') as file:
        for line in file:
            matrix.append(list(line.strip()))
    return matrix

def sum_of_part_numbers(matrix):
    sum = 0
    visited = [[False for _ in range(len(matrix[0]))] for _ in range(len(matrix))]
    
    for y, row in enumerate(matrix):
        for x, cell in enumerate(row):
            if not visited[y][x] and cell.isdigit():
                number, length = extract_number(matrix, x, y)
                if is_adjacent_to_symbol(matrix, x, y, length):
                    sum += number
                for i in range(length):
                    visited[y][x+i] = True
    return sum

def extract_number(matrix, x, y):
    number_str = ""
    while x < len(matrix[y]) and matrix[y][x].isdigit():
        number_str += matrix[y][x]
        x += 1
    return int(number_str), len(number_str)

def is_adjacent_to_symbol(matrix, x, y, length):
    for i in range(length):
        if check_adjacent(matrix, x+i, y):
            return True
    return False

def check_adjacent(matrix, x, y):
    for dy in range(-1, 2):
        for dx in range(-1, 2):
            adj_x, adj_y = x + dx, y + dy
            if 0 <= adj_y < len(matrix) and 0 <= adj_x < len(matrix[adj_y]):
                if not matrix[adj_y][adj_x].isdigit() and matrix[adj_y][adj_x] != '.':
                    return True
    return False

if __name__ == "__main__":
    input_file = "input.txt"
    matrix = read_input_file(input_file)
    result = sum_of_part_numbers(matrix)
    print(result)
