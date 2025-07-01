
import std.stdio;
import std.file;
import std.string;
import std.algorithm;

void main() {
    int[10][10] grid;
    auto lines = readText("input.txt").strip.splitLines;
    foreach (y, line; lines) {
        foreach (x, c; line) {
            grid[y][x] = c - '0';
        }
    }

    int step = 0;
    while (true) {
        step++;
        if (simulateStep(grid) == 100) {
            writeln(step);
            break;
        }
    }
}

int simulateStep(ref int[10][10] grid) {
    struct Point { int x, y; }
    Point[] toFlash;
    toFlash.reserve(20);

    foreach (y; 0 .. 10) {
        foreach (x; 0 .. 10) {
            grid[y][x]++;
            if (grid[y][x] > 9) {
                toFlash ~= Point(x, y);
            }
        }
    }

    size_t head = 0;
    while (head < toFlash.length) {
        auto p = toFlash[head++];
        foreach (dy; -1 .. 2) {
            foreach (dx; -1 .. 2) {
                if (dx == 0 && dy == 0) continue;
                int nx = p.x + dx;
                int ny = p.y + dy;

                if (nx >= 0 && nx < 10 && ny >= 0 && ny < 10) {
                    grid[ny][nx]++;
                    if (grid[ny][nx] == 10) {
                        toFlash ~= Point(nx, ny);
                    }
                }
            }
        }
    }

    foreach (p; toFlash) {
        grid[p.y][p.x] = 0;
    }

    return cast(int)toFlash.length;
}
