import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.math; // Added import for abs

struct Point {
    int x, y, z, w;
}

int manhattanDistance(Point a, Point b) {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.w - b.w);
}

void dfs(int index, bool[] visited, Point[] points) {
    visited[index] = true;
    foreach (i, point; points) {
        if (!visited[i] && manhattanDistance(points[index], point) <= 3) {
            dfs(cast(int)i, visited, points); // Cast i to int
        }
    }
}

void main() {
    string data = cast(string)std.file.read("input.txt");
    Point[] points;
    foreach (line; data.splitLines()) {
        auto coords = line.split(",");
        points ~= Point(to!int(coords[0]), to!int(coords[1]), to!int(coords[2]), to!int(coords[3]));
    }

    bool[] visited = new bool[points.length];
    int constellations = 0;

    foreach (i, point; points) {
        if (!visited[i]) {
            constellations++;
            dfs(cast(int)i, visited, points); // Cast i to int
        }
    }

    writeln(constellations);
}