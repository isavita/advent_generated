from itertools import product

def parse_input(input_str):
    tiles = {}
    for tile in input_str.strip().split('\n\n'):
        lines = tile.split('\n')
        tile_id = int(lines[0].split()[1][:-1])
        tiles[tile_id] = lines[1:]
    return tiles

def get_orientations(tile):
    orientations = []
    for flip in [False, True]:
        t = tile if not flip else [row[::-1] for row in tile]
        for rotation in range(4):
            orientations.append(t)
            t = [''.join(row) for row in zip(*t[::-1])]
    return [''.join(row) for orientation in orientations for row in orientation]

def find_corner_tiles(tiles):
    edge_matches = {tile_id: 0 for tile_id in tiles}
    
    for tile_id, tile in tiles.items():
        orientations = get_orientations(tile)
        for other_id, other_tile in tiles.items():
            if tile_id != other_id:
                other_orientations = get_orientations(other_tile)
                for edge in [orientations[0], orientations[-1]]:
                    if any(edge in [o[0], o[-1]] for o in other_orientations):
                        edge_matches[tile_id] += 1
                        break
    
    corner_tiles = [tile_id for tile_id, matches in edge_matches.items() if matches == 2]
    return corner_tiles

def solve(input_str):
    tiles = parse_input(input_str)
    corner_tiles = find_corner_tiles(tiles)
    return prod(corner_tiles)

from math import prod

# Example usage
input_str = """Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
"""

result = solve(input_str)
print(result)
