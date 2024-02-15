
import std.stdio;
import std.file;
import std.conv;

int countCombinations(int[] containers, int target, int index) {
    if (target == 0) {
        return 1;
    }
    if (target < 0 || index >= containers.length) {
        return 0;
    }
    return countCombinations(containers, target - containers[index], index + 1) +
           countCombinations(containers, target, index + 1);
}

void main() {
    int[] containers;
    foreach (line; File("input.txt").byLine()) {
        containers ~= to!int(line);
    }

    writeln(countCombinations(containers, 150, 0));
}
