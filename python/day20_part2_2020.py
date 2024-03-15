import math
import os

def parse_tiles_from_input(input_str):
    tiles = []
    for block in input_str.split("\n\n"):
        lines = block.split("\n")
        tile_id = int(lines[0].split(" ")[1][:-1])
        contents = [list(line) for line in lines[1:]]
        tiles.append({"id": tile_id, "contents": contents})
    return tiles

def get_col(grid, first_col):
    return "".join([grid[i][0] if first_col else grid[i][-1] for i in range(len(grid))])

def get_row(grid, first_row):
    return "".join(grid[0] if first_row else grid[-1])

def remove_borders_from_grid(grid):
    return [row[1:-1] for row in grid[1:-1]]

def backtrack_assemble(tiles, assembled_tiles, used_indices):
    edge_size = int(math.sqrt(len(tiles)))
    if assembled_tiles is None:
        assembled_tiles = [[None for _ in range(edge_size)] for _ in range(edge_size)]

    for row in range(edge_size):
        for col in range(edge_size):
            if assembled_tiles[row][col] is None:
                for i, t in enumerate(tiles):
                    if i not in used_indices:
                        for opt in all_grid_orientations(t["contents"]):
                            if row != 0:
                                current_top_row = get_row(opt, True)
                                bottom_of_above = get_row(assembled_tiles[row-1][col]["contents"], False)
                                if current_top_row != bottom_of_above:
                                    continue
                            if col != 0:
                                current_left_col = get_col(opt, True)
                                right_col_of_left = get_col(assembled_tiles[row][col-1]["contents"], False)
                                if current_left_col != right_col_of_left:
                                    continue

                            t["contents"] = opt
                            assembled_tiles[row][col] = t
                            used_indices.add(i)
                            result = backtrack_assemble(tiles, assembled_tiles, used_indices)
                            if result is not None:
                                return result
                            assembled_tiles[row][col] = None
                            used_indices.remove(i)

                if assembled_tiles[row][col] is None:
                    return None

    return assembled_tiles

def all_grid_orientations(grid):
    orientations = [grid]
    for _ in range(3):
        orientations.append(rotate_string_grid(orientations[-1]))
    for i in range(4):
        orientations.append(mirror_string_grid(orientations[i]))
    return orientations

def rotate_string_grid(grid):
    return [list("".join(row)) for row in zip(*grid[::-1])]

def mirror_string_grid(grid):
    return [list(row[::-1]) for row in grid]

def find_monster_coords(image):
    monster = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    ]
    monster_offsets = [(r, c) for r, line in enumerate(monster) for c, char in enumerate(line) if char == "#"]
    monster_height, monster_length = len(monster), len(monster[0])

    monster_starting_coords = []
    for r in range(len(image) - monster_height + 1):
        for c in range(len(image[0]) - monster_length + 1):
            monster_found = True
            for dr, dc in monster_offsets:
                if image[r+dr][c+dc] != "#":
                    monster_found = False
                    break
            if monster_found:
                monster_starting_coords.append((r, c))

    monster_coords = []
    for r, c in monster_starting_coords:
        for dr, dc in monster_offsets:
            monster_coords.append((r+dr, c+dc))

    return monster_coords

def solve(input_str):
    tiles = parse_tiles_from_input(input_str)
    edge_size = int(math.sqrt(len(tiles)))

    assembled_tiles = backtrack_assemble(tiles, None, set())

    for row in assembled_tiles:
        for cell in row:
            cell["contents"] = remove_borders_from_grid(cell["contents"])

    image = []
    for big_row in range(edge_size):
        for sub_row in range(len(assembled_tiles[0][0]["contents"])):
            image.append([])
            for big_col in range(edge_size):
                sub_line = assembled_tiles[big_row][big_col]["contents"][sub_row]
                image[-1].extend(sub_line)

    for opt in all_grid_orientations(image):
        monster_coords = find_monster_coords(opt)
        if monster_coords:
            image = opt
            break

    for r, c in monster_coords:
        image[r][c] = "O"

    rough_waters_count = sum(row.count("#") for row in image)
    return rough_waters_count

with open("input.txt", "r") as f:
    input_str = f.read().strip()

print(solve(input_str))