
import sys

def enhance(input, rules):
    for i in range(4):
        if input in rules:
            return rules[input]
        input = rotate(input)
    input = flip(input)
    for i in range(4):
        if input in rules:
            return rules[input]
        input = rotate(input)
    return ""

def rotate(input):
    parts = input.split("/")
    size = len(parts)
    newParts = ["" for _ in range(size)]
    for x in range(size):
        newRow = ""
        for y in range(size-1, -1, -1):
            newRow += parts[y][x]
        newParts[x] = newRow
    return "/".join(newParts)

def flip(input):
    parts = input.split("/")
    for i, part in enumerate(parts):
        parts[i] = part[::-1]
    return "/".join(parts)

def main():
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

    for i in range(5):
        if len(grid) % 2 == 0:
            subSize = 2
            newSize = len(grid) // 2 * 3
        else:
            subSize = 3
            newSize = len(grid) // 3 * 4

        newGrid = [""] * newSize

        for y in range(0, len(grid), subSize):
            for x in range(0, len(grid), subSize):
                square = [grid[y+dy][x:x+subSize] for dy in range(subSize)]
                newSquare = enhance("/".join(square), rules)
                for dy, row in enumerate(newSquare.split("/")):
                    newGrid[y//subSize*(subSize+1)+dy] += row

        grid = newGrid

    count = 0
    for row in grid:
        for pixel in row:
            if pixel == '#':
                count += 1
    print(count)

if __name__ == "__main__":
    main()
