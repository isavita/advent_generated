
import std.stdio;
import std.file;
import std.conv;
import std.array;

struct Reindeer {
    int speed;
    int flyTime;
    int restTime;
    int distance;
    int points;
    bool flying;
    int timeInMode;
}

void main() {
    auto reindeers = readReindeerDetails("input.txt");
    simulateRaceWithPoints(reindeers, 2503);
    int maxPoints = findMaxPoints(reindeers);
    writeln(maxPoints);
}

Reindeer[] readReindeerDetails(string filename) {
    auto file = File(filename, "r");
    Reindeer[] reindeers;

    foreach (line; file.byLine()) {
        auto parts = line.split(" ");
        int speed = to!int(parts[3]);
        int flyTime = to!int(parts[6]);
        int restTime = to!int(parts[13]);

        reindeers ~= Reindeer(speed, flyTime, restTime, 0, 0, true, 0);
    }

    return reindeers;
}

void simulateRaceWithPoints(Reindeer[] reindeers, int totalSeconds) {
    foreach (i; 0 .. totalSeconds) {
        int maxDistance = 0;
        foreach (j, ref reindeer; reindeers) {
            if (reindeer.flying) {
                reindeer.distance += reindeer.speed;
            }
            reindeer.timeInMode++;
            if ((reindeer.flying && reindeer.timeInMode == reindeer.flyTime) || (!reindeer.flying && reindeer.timeInMode == reindeer.restTime)) {
                reindeer.flying = !reindeer.flying;
                reindeer.timeInMode = 0;
            }
            if (reindeer.distance > maxDistance) {
                maxDistance = reindeer.distance;
            }
        }
        foreach (j, ref reindeer; reindeers) {
            if (reindeer.distance == maxDistance) {
                reindeer.points++;
            }
        }
    }
}

int findMaxPoints(Reindeer[] reindeers) {
    int maxPoints = 0;
    foreach (reindeer; reindeers) {
        if (reindeer.points > maxPoints) {
            maxPoints = reindeer.points;
        }
    }
    return maxPoints;
}
