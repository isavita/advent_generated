import std.stdio;
import std.conv;
import std.file;
import std.array;
import std.algorithm;
import std.string;
import std.typecons; // For Tuple
import std.math; // For abs function

// Define a type for coordinates with steps
struct CoordSteps {
    int x;
    int y;
    int steps;
}

CoordSteps[] tracePath(string path) {
    CoordSteps[] coordinates;
    int x = 0, y = 0, steps = 0;
    foreach (move; path.split(",")) {
        char direction = move[0];
        int distance = to!int(move[1..$]);
        foreach (_; 0 .. distance) {
            ++steps; // Increment steps for each move
            switch (direction) {
                case 'U': ++y; break;
                case 'D': --y; break;
                case 'L': --x; break;
                case 'R': ++x; break;
                default: assert(0, "Unknown direction");
            }
            coordinates ~= CoordSteps(x, y, steps);
        }
    }
    return coordinates;
}

void main() {
    auto lines = readText("input.txt").strip().split("\n");

    auto wire1Path = tracePath(lines[0]);
    auto wire2Path = tracePath(lines[1]);

    // Convert wire1 path to an associative array for quick lookup by coordinate
    int[string] wire1Steps;
    foreach (coord; wire1Path) {
        auto key = format("%d,%d", coord.x, coord.y);
        if (key !in wire1Steps) { // Correctly check if key does not exist
            wire1Steps[key] = coord.steps;
        }
    }

    // Find intersections and calculate steps for wire2
    int[] totalSteps;
    foreach (coord; wire2Path) {
        auto key = format("%d,%d", coord.x, coord.y);
        if (auto stepPtr = key in wire1Steps) { // Check if the key exists and retrieve the pointer
            totalSteps ~= *stepPtr + coord.steps; // Dereference the pointer to get the value
        }
    }

    if (!totalSteps.empty) {
        int fewestSteps = totalSteps.minElement;
        writeln(fewestSteps);
    } else {
        writeln("No intersections found.");
    }
}

