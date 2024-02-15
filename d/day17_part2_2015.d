
import std.stdio;
import std.file;
import std.conv;

void findCombinations(int[] containers, int target, int index, int count, ref int minCount, ref int ways) {
    if (target == 0) {
        if (minCount == 0 || count < minCount) {
            minCount = count;
            ways = 1;
        } else if (count == minCount) {
            ways++;
        }
        return;
    }
    if (target < 0 || index >= containers.length) {
        return;
    }
    // Include current container
    findCombinations(containers, target - containers[index], index + 1, count + 1, minCount, ways);
    // Exclude current container
    findCombinations(containers, target, index + 1, count, minCount, ways);
}

void main() {
    int[] containers;
    foreach (line; File("input.txt").byLine()) {
        containers ~= to!int(line);
    }

    int minCount = 0;
    int ways = 0;
    findCombinations(containers, 150, 0, 0, minCount, ways);
    writeln(ways);
}
