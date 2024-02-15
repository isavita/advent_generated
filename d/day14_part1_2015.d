
import std.file : readText;
import std.conv : to;
import std.algorithm : max, min; // Added import for min
import std.array : split;
import std.string : chomp;
import std.stdio : writeln; // Added import for writeln

struct Reindeer {
    string name;
    int speed;
    int flyTime;
    int restTime;
}

void main() {
    string input = readText("input.txt").chomp;
    string[] lines = input.split("\n");

    Reindeer[] reindeers;

    foreach (line; lines) {
        string[] parts = line.split(" ");
        reindeers ~= Reindeer(parts[0], parts[3].to!int, parts[6].to!int, parts[13].to!int);
    }

    int maxDistance = 0;

    foreach (reindeer; reindeers) {
        int cycleTime = reindeer.flyTime + reindeer.restTime;
        int fullCycles = 2503 / cycleTime;
        int remainingTime = 2503 % cycleTime;

        int distance = fullCycles * reindeer.speed * reindeer.flyTime;
        distance += min(remainingTime, reindeer.flyTime) * reindeer.speed;

        maxDistance = max(maxDistance, distance);
    }

    writeln(maxDistance);
}
