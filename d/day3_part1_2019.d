import std.stdio;
import std.conv;
import std.file;
import std.array;
import std.algorithm;
import std.string;
import std.typecons; // For Tuple
import std.math; // For abs function

// Define a type for coordinates
alias Coord = Tuple!(int, "x", int, "y");

Coord[] tracePath(string path) {
    Coord[] coordinates;
    int x = 0, y = 0;
    foreach (move; path.split(",")) {
        char direction = move[0];
        int distance = to!int(move[1..$]);
        foreach (_; 0 .. distance) {
            switch (direction) {
                case 'U': ++y; break;
                case 'D': --y; break;
                case 'L': --x; break;
                case 'R': ++x; break;
                default: assert(0, "Unknown direction"); // Correct handling of unexpected cases
            }
            coordinates ~= Coord(x, y);
        }
    }
    return coordinates;
}

int manhattanDistance(Coord coord) {
    return abs(coord.x) + abs(coord.y); // abs now available from std.math
}

void main() {
    auto lines = readText("input.txt").strip().split("\n");

    auto wire1Path = tracePath(lines[0]);
    auto wire2Path = tracePath(lines[1]);

    // Convert paths to associative arrays for quick lookup
    bool[Coord] wire1Coords;
    foreach (coord; wire1Path) {
        wire1Coords[coord] = true;
    }

    // Find intersections
    int[] distances;
    foreach (coord; wire2Path) {
        if (wire1Coords.get(coord, false)) {
            distances ~= manhattanDistance(coord);
        }
    }

    if (!distances.empty) {
        int minDistance = distances.minElement;
        writeln(minDistance);
    } else {
        writeln("No intersections found.");
    }
}

