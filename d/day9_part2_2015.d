
import std.stdio, std.file, std.string, std.conv, std.algorithm, std.range;

void main() {
    int[string[2]] distances;
    string[] allLocations;

    foreach (line; readText("input.txt").splitLines()) {
        if (line.empty) continue;
        auto parts = line.split();
        auto loc1 = parts[0];
        auto loc2 = parts[2];
        auto dist = to!int(parts[4]);

        distances[[loc1, loc2]] = dist;
        distances[[loc2, loc1]] = dist;
        allLocations ~= loc1;
        allLocations ~= loc2;
    }

    auto locations = allLocations.sort().uniq().array();

    int shortest = int.max;
    int longest = 0;

    foreach (perm; locations.permutations()) {
        auto currentDistance = zip(perm[0 .. $ - 1], perm[1 .. $])
                               .map!(pair => distances[[pair[0], pair[1]]])
                               .sum();
        shortest = min(shortest, currentDistance);
        longest = max(longest, currentDistance);
    }

    writeln(shortest);
    writeln(longest);
}
