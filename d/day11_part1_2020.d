
import std.stdio;
import std.file;
import std.string;
import std.array;
import std.algorithm;
import std.range;

void main() {
    auto currentGrid = readText("input.txt").strip.splitLines.map!(a => a.dup).array;
    if (currentGrid.empty) {
        writeln(0);
        return;
    }
    auto nextGrid = currentGrid.map!(a => a.dup).array;

    auto rows = currentGrid.length;
    auto cols = currentGrid[0].length;

    while (true) {
        bool changed = false;
        foreach (r; 0 .. rows) {
            foreach (c; 0 .. cols) {
                char seat = currentGrid[r][c];
                if (seat == '.') {
                    nextGrid[r][c] = '.';
                    continue;
                }

                int adjacent = 0;
                foreach (dr; -1 .. 2) {
                    foreach (dc; -1 .. 2) {
                        if (dr == 0 && dc == 0) continue;
                        auto nr = cast(ptrdiff_t)r + dr;
                        auto nc = cast(ptrdiff_t)c + dc;
                        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && currentGrid[nr][nc] == '#') {
                            adjacent++;
                        }
                    }
                }

                if (seat == 'L' && adjacent == 0) {
                    nextGrid[r][c] = '#';
                    changed = true;
                } else if (seat == '#' && adjacent >= 4) {
                    nextGrid[r][c] = 'L';
                    changed = true;
                } else {
                    nextGrid[r][c] = seat;
                }
            }
        }

        if (!changed) break;
        swap(currentGrid, nextGrid);
    }

    writeln(currentGrid.map!(row => row.count('#')).sum);
}
