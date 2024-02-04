
import sys

memo = {}

def enhance(input_str, rules):
    if input_str in memo:
        return memo[input_str]

    original = input_str
    for i in range(4):
        if input_str in rules:
            output = rules[input_str]
            memo[original] = output
            return output
        input_str = rotate(input_str)
    input_str = flip(input_str)
    for i in range(4):
        if input_str in rules:
            output = rules[input_str]
            memo[original] = output
            return output
        input_str = rotate(input_str)
    return ""

def rotate(input_str):
    parts = input_str.split("/")
    size = len(parts)
    new_parts = [''] * size
    for x in range(size):
        new_row = ""
        for y in range(size - 1, -1, -1):
            new_row += parts[y][x]
        new_parts[x] = new_row
    return "/".join(new_parts)

def flip(input_str):
    parts = input_str.split("/")
    for i in range(len(parts)):
        parts[i] = parts[i][::-1]
    return "/".join(parts)

def reverse(input_str):
    return input_str[::-1]

if __name__ == "__main__":
    rules = {}
    with open("input.txt", "r") as file:
        for line in file:
            parts = line.strip().split(" => ")
            rules[parts[0]] = parts[1]

    grid = [
        ".#.",
        "..#",
        "###",
    ]

    for i in range(18):
        if len(grid) % 2 == 0:
            sub_size = 2
            new_size = len(grid) // 2 * 3
        else:
            sub_size = 3
            new_size = len(grid) // 3 * 4

        new_grid = [""] * new_size

        for y in range(0, len(grid), sub_size):
            for x in range(0, len(grid), sub_size):
                square = [grid[y+dy][x:x+sub_size] for dy in range(sub_size)]
                new_square = enhance("/".join(square), rules)
                for dy, row in enumerate(new_square.split("/")):
                    new_grid[y//sub_size*(sub_size+1)+dy] += row

        grid = new_grid

    count = 0
    for row in grid:
        for pixel in row:
            if pixel == '#':
                count += 1
    print(count)
