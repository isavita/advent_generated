
import std.stdio;
import std.file;
import std.string;
import std.format;

void main() {
    bool[101][101][101] grid;

    foreach (line; readText("input.txt").splitLines()) {
        if (line.empty) continue;

        string action;
        int x1, x2, y1, y2, z1, z2;
        formattedRead(line, "%s x=%d..%d,y=%d..%d,z=%d..%d",
                      &action, &x1, &x2, &y1, &y2, &z1, &z2);

        if (x1 < -50 || x2 > 50 || y1 < -50 || y2 > 50 || z1 < -50 || z2 > 50) {
            continue;
        }

        bool state = (action == "on");
        foreach (x; x1 .. x2 + 1) {
            foreach (y; y1 .. y2 + 1) {
                foreach (z; z1 .. z2 + 1) {
                    grid[x + 50][y + 50][z + 50] = state;
                }
            }
        }
    }

    long onCubes = 0;
    foreach (plane; grid) {
        foreach (row; plane) {
            foreach (cell; row) {
                if (cell) {
                    onCubes++;
                }
            }
        }
    }

    writeln(onCubes);
}
