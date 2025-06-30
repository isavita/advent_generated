
import std.stdio;
import std.file;
import std.string;

struct Coord {
    int x, y;

    Coord opBinary(string op)(Coord other) const if (op == "+") {
        return Coord(x + other.x, y + other.y);
    }

    bool opEquals(const Coord other) const {
        return x == other.x && y == other.y;
    }

    size_t toHash() const @safe {
        size_t h = x;
        h = (h << 5) - h + y;
        return h;
    }
}

struct Grid {
    int width, height;
    bool[Coord] rocks;

    bool isInBounds(Coord c) const {
        return c.x >= 0 && c.x < width && c.y >= 0 && c.y < height;
    }
}

void main() {
    auto lines = readText("input.txt").strip().splitLines();
    const int numSteps = 64;

    Grid grid;
    Coord start;
    grid.height = cast(int)lines.length;
    grid.width = cast(int)lines[0].length;

    foreach (y, line; lines) {
        foreach (x, char c; line) {
            auto coord = Coord(cast(int)x, cast(int)y);
            if (c == '#') {
                grid.rocks[coord] = true;
            } else if (c == 'S') {
                start = coord;
            }
        }
    }

    auto frontier = [start];
    auto reached = [start: 0];
    size_t head = 0;

    while (head < frontier.length) {
        auto current = frontier[head++];
        int currentDist = reached[current];

        if (currentDist >= numSteps) continue;

        const directions = [Coord(0, -1), Coord(0, 1), Coord(1, 0), Coord(-1, 0)];
        foreach (dir; directions) {
            auto next = current + dir;
            if (grid.isInBounds(next) && !(next in grid.rocks) && !(next in reached)) {
                reached[next] = currentDist + 1;
                frontier ~= next;
            }
        }
    }

    long count = 0;
    foreach (dist; reached.values) {
        if (dist % 2 == numSteps % 2) {
            count++;
        }
    }
    writeln(count);
}
