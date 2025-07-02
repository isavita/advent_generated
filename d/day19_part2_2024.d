
import std.stdio;
import std.string;
import std.file;
import std.array;
import std.algorithm;

long countWays(string design, string[] patterns) {
    long[string] memo;

    long recurse(string remaining) {
        if (remaining.empty) {
            return 1;
        }
        if (auto pval = remaining in memo) {
            return *pval;
        }
        long count = 0;
        foreach (pattern; patterns) {
            if (remaining.startsWith(pattern)) {
                count += recurse(remaining[pattern.length .. $]);
            }
        }
        return memo[remaining] = count;
    }

    return recurse(design);
}

void main() {
    auto lines = readText("input.txt").splitLines()
        .filter!(a => !a.strip.empty).array;

    auto patterns = lines[0].split(", ").map!(a => a.strip).array;

    long totalWays = 0;
    foreach (design; lines[1 .. $]) {
        totalWays += countWays(design, patterns);
    }

    writeln(totalWays);
}
