
from collections import defaultdict

def rotate(tile):
    return [''.join(x) for x in zip(*reversed(tile))]

def flip(tile):
    return [x[::-1] for x in tile]

def get_borders(tile):
    return [tile[0], tile[-1], ''.join([x[0] for x in tile]), ''.join([x[-1] for x in tile])]

def solve(input_file):
    tiles = {}
    with open(input_file, 'r') as file:
        lines = file.read().strip().split('\n')
    
    for line in lines:
        if 'Tile' in line:
            tile_id = int(line.split()[1][:-1])
            tiles[tile_id] = []
        elif line:
            tiles[tile_id].append(line)
    
    borders = defaultdict(list)
    for tile_id, tile in tiles.items():
        for border in get_borders(tile):
            borders[border].append(tile_id)
            borders[border[::-1]].append(tile_id)
    
    corner_tiles = []
    for tile_id, tile in tiles.items():
        matching_borders = 0
        for border in get_borders(tile):
            if len(borders[border]) == 1:
                matching_borders += 1
        if matching_borders == 2:
            corner_tiles.append(tile_id)
    
    return corner_tiles[0] * corner_tiles[1] * corner_tiles[2] * corner_tiles[3]

print(solve('input.txt'))
