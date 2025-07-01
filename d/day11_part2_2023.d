
import std.stdio, std.file, std.string, std.algorithm, std.array, std.range;

struct Coord {
    long x, y;
}

void main() {
    auto lines = readText("input.txt").strip.splitLines;
    if (lines.empty) {
        writeln(0);
        return;
    }

    immutable height = lines.length;
    immutable width = lines[0].length;

    auto galaxies = appender!(Coord[]);
    auto rowHasGalaxy = new bool[height];
    auto colHasGalaxy = new bool[width];

    foreach (y, line; lines) {
        foreach (x, char c; line) {
            if (c == '#') {
                galaxies.put(Coord(x, y));
                rowHasGalaxy[y] = true;
                colHasGalaxy[x] = true;
            }
        }
    }

    auto initialGalaxies = galaxies.data;
    if (initialGalaxies.length < 2) {
        writeln(0);
        return;
    }

    auto emptyRowsBefore = new long[height];
    long emptyCount = 0;
    foreach (y; 0 .. height) {
        emptyRowsBefore[y] = emptyCount;
        if (!rowHasGalaxy[y]) emptyCount++;
    }

    auto emptyColsBefore = new long[width];
    emptyCount = 0;
    foreach (x; 0 .. width) {
        emptyColsBefore[x] = emptyCount;
        if (!colHasGalaxy[x]) emptyCount++;
    }

    immutable expansionAmount = 1_000_000 - 1;

    auto expandedGalaxies = initialGalaxies.map!(g =>
        Coord(g.x + emptyColsBefore[g.x] * expansionAmount,
              g.y + emptyRowsBefore[g.y] * expansionAmount)
    ).array;

    long totalDistance = 0;

    expandedGalaxies.sort!((a, b) => a.x < b.x);
    long currentSumX = 0;
    foreach (i, g; expandedGalaxies) {
        totalDistance += i * g.x - currentSumX;
        currentSumX += g.x;
    }

    expandedGalaxies.sort!((a, b) => a.y < b.y);
    long currentSumY = 0;
    foreach (i, g; expandedGalaxies) {
        totalDistance += i * g.y - currentSumY;
        currentSumY += g.y;
    }

    writeln(totalDistance);
}
