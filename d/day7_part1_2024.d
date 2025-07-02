
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;
import std.algorithm;

bool canBeTrue(long testValue, int[] numbers) {
    if (numbers.empty) {
        return false;
    }

    bool solve(size_t index, long currentResult) {
        if (index == numbers.length) {
            return currentResult == testValue;
        }
        return solve(index + 1, currentResult + numbers[index]) ||
               solve(index + 1, currentResult * numbers[index]);
    }

    return solve(1, numbers[0]);
}

void main() {
    long totalCalibrationResult = 0;
    foreach (line; readText("input.txt").splitLines()) {
        if (line.length == 0) continue;

        auto parts = line.split(":");
        auto testValue = parts[0].to!long;
        auto numbers = parts[1].strip.split(" ").map!(a => a.to!int).array;

        if (canBeTrue(testValue, numbers)) {
            totalCalibrationResult += testValue;
        }
    }
    writeln(totalCalibrationResult);
}
