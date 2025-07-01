
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.array;

void main() {
    auto heightmap = readText("input.txt")
        .splitLines()
        .filter!(a => !a.empty)
        .map!(row => row.map!(c => c - '0').array)
        .array;

    long totalRiskLevel = 0;
    auto height = heightmap.length;
    auto width = heightmap[0].length;

    foreach (y; 0 .. height) {
        foreach (x; 0 .. width) {
            auto current = heightmap[y][x];
            if ((x == 0 || heightmap[y][x - 1] > current) &&
                (x == width - 1 || heightmap[y][x + 1] > current) &&
                (y == 0 || heightmap[y - 1][x] > current) &&
                (y == height - 1 || heightmap[y + 1][x] > current)) {
                totalRiskLevel += 1 + current;
            }
        }
    }

    writeln(totalRiskLevel);
}
