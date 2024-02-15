
import std.stdio;
import std.file;
import std.conv;
import std.regex;

struct Nanobot {
    int x, y, z, radius;
}

void main() {
    auto file = File("input.txt", "r");
    auto nanobots = parseNanobots(file);

    auto strongest = findStrongestNanobot(nanobots);
    auto inRangeCount = countNanobotsInRange(nanobots, strongest);

    writeln(inRangeCount);
}

Nanobot[] parseNanobots(File file) {
    Nanobot[] nanobots;
    auto re = ctRegex!"pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)";
    
    foreach (line; file.byLine()) {
        auto matches = matchFirst(line, re);
        
        int x = to!int(matches[1]);
        int y = to!int(matches[2]);
        int z = to!int(matches[3]);
        int radius = to!int(matches[4]);

        nanobots ~= Nanobot(x, y, z, radius);
    }

    return nanobots;
}

Nanobot findStrongestNanobot(Nanobot[] nanobots) {
    Nanobot strongest;
    foreach (nanobot; nanobots) {
        if (nanobot.radius > strongest.radius) {
            strongest = nanobot;
        }
    }
    return strongest;
}

int countNanobotsInRange(Nanobot[] nanobots, Nanobot strongest) {
    int count = 0;
    foreach (nanobot; nanobots) {
        if (manhattanDistance(nanobot, strongest) <= strongest.radius) {
            count++;
        }
    }
    return count;
}

int manhattanDistance(Nanobot a, Nanobot b) {
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z);
}

int abs(int x) {
    return x < 0 ? -x : x;
}
