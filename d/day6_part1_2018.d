
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.math;
import std.array;

struct Point {
    int x;
    int y;
}

void main() {
    auto points = readText("input.txt")
                  .strip
                  .splitLines
                  .map!(line => line.split(", ").to!(int[]))
                  .map!(coords => Point(coords[0], coords[1]))
                  .array;

    auto maxX = points.map!(p => p.x).maxElement;
    auto maxY = points.map!(p => p.y).maxElement;

    auto gridWidth = maxX + 2;
    auto gridHeight = maxY + 2;

    auto areas = new int[points.length];
    auto infinite = new bool[points.length];

    foreach (i; 0 .. gridWidth) {
        foreach (j; 0 .. gridHeight) {
            int minDist = int.max;
            int owner = -1;

            foreach (k, ref p; points) {
                auto dist = abs(p.x - i) + abs(p.y - j);
                if (dist < minDist) {
                    minDist = dist;
                    owner = cast(int)k;
                } else if (dist == minDist) {
                    owner = -1;
                }
            }

            if (owner != -1) {
                if (i == 0 || j == 0 || i == gridWidth - 1 || j == gridHeight - 1) {
                    infinite[owner] = true;
                }
                areas[owner]++;
            }
        }
    }

    int maxArea = 0;
    foreach (i, area; areas) {
        if (!infinite[i] && area > maxArea) {
            maxArea = area;
        }
    }

    writeln(maxArea);
}
