
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.array;

bool canBeMadeTrue(long testValue, long[] numbers) {
    if (numbers.empty) return false;
    if (numbers.length == 1) return testValue == numbers[0];

    size_t numOperators = numbers.length - 1;
    long combinations = 1;
    foreach (_; 0 .. numOperators) {
        combinations *= 3;
    }

    for (long i = 0; i < combinations; i++) {
        long currentCombination = i;
        long result = numbers[0];
        for (size_t j = 0; j < numOperators; j++) {
            long nextNumber = numbers[j + 1];
            switch (currentCombination % 3) {
                case 0:
                    result += nextNumber;
                    break;
                case 1:
                    result *= nextNumber;
                    break;
                case 2:
                    long multiplier = 10;
                    if (nextNumber > 0) {
                        long temp = nextNumber;
                        while (temp >= 10) {
                            multiplier *= 10;
                            temp /= 10;
                        }
                    }
                    result = result * multiplier + nextNumber;
                    break;
                default:
                    break;
            }
            currentCombination /= 3;
        }
        if (result == testValue) return true;
    }
    return false;
}

void main() {
    long total = 0;
    foreach (line; readText("input.txt").splitLines) {
        if (line.empty) continue;
        auto parts = line.split(':');
        long testValue = parts[0].to!long;
        auto numbers = parts[1].strip.split(' ').map!(s => s.to!long).array;
        if (canBeMadeTrue(testValue, numbers)) {
            total += testValue;
        }
    }
    writeln(total);
}
