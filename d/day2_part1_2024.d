
import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.range;

bool isSafeReport(const int[] levels) {
    if (levels.length < 2) {
        return false;
    }
    auto firstDiff = levels[1] - levels[0];
    if (firstDiff == 0) {
        return false;
    }
    bool isIncreasing = firstDiff > 0;
    for (size_t i = 0; i < levels.length - 1; i++) {
        auto diff = levels[i + 1] - levels[i];
        if (isIncreasing) {
            if (diff < 1 || diff > 3) return false;
        } else {
            if (diff > -1 || diff < -3) return false;
        }
    }
    return true;
}

void main() {
    writeln(File("input.txt")
        .byLineCopy
        .filter!(a => !a.strip.empty)
        .map!(a => a.split.map!(to!int).array)
        .filter!isSafeReport
        .walkLength);
}
