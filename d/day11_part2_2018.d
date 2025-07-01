
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.numeric;

void main() {
    immutable gridSize = 300;
    auto serial = readText("input.txt").strip.to!int;

    int[gridSize + 1][gridSize + 1] sat;

    foreach (y; 1 .. gridSize + 1) {
        foreach (x; 1 .. gridSize + 1) {
            auto rackID = x + 10;
            auto powerLevel = rackID * y;
            powerLevel += serial;
            powerLevel *= rackID;
            powerLevel = (powerLevel / 100) % 10;
            powerLevel -= 5;
            sat[y][x] = powerLevel + sat[y - 1][x] + sat[y][x - 1] - sat[y - 1][x - 1];
        }
    }

    auto maxPower = int.min;
    int maxX, maxY, maxSize;

    foreach (size; 1 .. gridSize + 1) {
        foreach (y; 1 .. gridSize - size + 2) {
            foreach (x; 1 .. gridSize - size + 2) {
                auto totalPower = sat[y + size - 1][x + size - 1] - sat[y - 1][x + size - 1] - sat[y + size - 1][x - 1] + sat[y - 1][x - 1];
                if (totalPower > maxPower) {
                    maxPower = totalPower;
                    maxX = x;
                    maxY = y;
                    maxSize = size;
                }
            }
        }
    }

    writeln(maxX, ",", maxY, ",", maxSize);
}
