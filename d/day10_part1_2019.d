
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.numeric;
import std.math;

struct Point {
    int x, y;
}

void main() {
    auto lines = readText("input.txt").strip.splitLines;

    Point[] asteroids;
    foreach (y, line; lines) {
        foreach (x, char c; line) {
            if (c == '#') {
                asteroids ~= Point(cast(int)x, cast(int)y);
            }
        }
    }

    int maxVisible = 0;
    foreach (station; asteroids) {
        bool[Point] directions;
        foreach (target; asteroids) {
            if (station == target) {
                continue;
            }

            int dx = target.x - station.x;
            int dy = target.y - station.y;
            
            auto common = gcd(abs(dx), abs(dy));
            
            directions[Point(dx / common, dy / common)] = true;
        }
        maxVisible = max(maxVisible, cast(int)directions.length);
    }

    writeln(maxVisible);
}
