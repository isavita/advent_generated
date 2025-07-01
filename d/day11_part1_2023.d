
import std.stdio;
import std.file;
import std.string;
import std.array;
import std.math;
import std.algorithm;

struct Coord {
    long x, y;
}

void main() {
    auto lines = readText("input.txt").strip().splitLines();
    if (lines.empty) {
        writeln(0);
        return;
    }

    auto height = lines.length;
    auto width = lines[0].length;

    auto galaxies = appender!(Coord[]);
    auto rowHasGalaxy = new bool[height];
    auto colHasGalaxy = new bool[width];

    foreach (y, line; lines) {
        foreach (x, c; line) {
            if (c == '#') {
                galaxies.put(Coord(x, y));
                rowHasGalaxy[y] = true;
                colHasGalaxy[x] = true;
            }
        }
    }

    auto yOffsets = new long[height];
    long currentOffset = 0;
    foreach (y, hasGalaxy; rowHasGalaxy) {
        if (!hasGalaxy) {
            currentOffset++;
        }
        yOffsets[y] = currentOffset;
    }

    auto xOffsets = new long[width];
    currentOffset = 0;
    foreach (x, hasGalaxy; colHasGalaxy) {
        if (!hasGalaxy) {
            currentOffset++;
        }
        xOffsets[x] = currentOffset;
    }

    long totalDistance = 0;
    immutable long expansion = 1;
    auto galaxyArr = galaxies.data;

    for (size_t i = 0; i < galaxyArr.length; ++i) {
        for (size_t j = i + 1; j < galaxyArr.length; ++j) {
            auto g1 = galaxyArr[i];
            auto g2 = galaxyArr[j];

            long x1 = g1.x + xOffsets[g1.x] * expansion;
            long y1 = g1.y + yOffsets[g1.y] * expansion;
            long x2 = g2.x + xOffsets[g2.x] * expansion;
            long y2 = g2.y + yOffsets[g2.y] * expansion;

            totalDistance += abs(x2 - x1) + abs(y2 - y1);
        }
    }

    writeln(totalDistance);
}
