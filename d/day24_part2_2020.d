
import std.stdio, std.file, std.string;

struct Coord { int x, y, z; }

void main() {
    bool[Coord] blackTiles;

    foreach (line; readText("input.txt").strip.splitLines) {
        int x = 0, y = 0, z = 0;
        for (size_t i = 0; i < line.length; ) {
            final switch (line[i++]) {
                case 'e': x++; y--; break;
                case 'w': x--; y++; break;
                case 'n':
                    if (line[i++] == 'e') { x++; z--; }
                    else { y++; z--; }
                    break;
                case 's':
                    if (line[i++] == 'e') { y--; z++; }
                    else { x--; z++; }
                    break;
            }
        }
        auto coord = Coord(x, y, z);
        if (coord in blackTiles) blackTiles.remove(coord);
        else blackTiles[coord] = true;
    }

    static immutable Coord[6] dirs = [
        Coord(1, -1, 0), Coord(-1, 1, 0), Coord(1, 0, -1),
        Coord(0, 1, -1), Coord(0, -1, 1), Coord(-1, 0, 1)
    ];

    foreach (_; 0 .. 100) {
        auto newTiles = typeof(blackTiles).init;
        int[Coord] whiteNeighborCounts;

        foreach (coord; blackTiles.keys) {
            int blackNeighbors = 0;
            foreach (dir; dirs) {
                auto neighbor = Coord(coord.x + dir.x, coord.y + dir.y, coord.z + dir.z);
                if (neighbor in blackTiles) {
                    blackNeighbors++;
                } else {
                    whiteNeighborCounts[neighbor]++;
                }
            }
            if (blackNeighbors == 1 || blackNeighbors == 2) {
                newTiles[coord] = true;
            }
        }

        foreach (coord, count; whiteNeighborCounts) {
            if (count == 2) {
                newTiles[coord] = true;
            }
        }
        blackTiles = newTiles;
    }

    writeln(blackTiles.length);
}
