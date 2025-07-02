
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.range;

void main() {
    auto parts = readText("input.txt").strip().split("\n\n");
    auto template_ = parts[0];
    string[string] rules;
    foreach (line; parts[1].splitLines()) {
        auto ruleParts = line.split(" -> ");
        rules[ruleParts[0]] = ruleParts[1];
    }

    long[string] pairCounts;
    foreach (i; 0 .. template_.length - 1) {
        pairCounts[template_[i .. i + 2]]++;
    }

    foreach (step; 0 .. 40) {
        long[string] newPairCounts;
        foreach (pair, count; pairCounts) {
            auto insert = rules[pair];
            newPairCounts[pair[0] ~ insert] += count;
            newPairCounts[insert ~ pair[1]] += count;
        }
        pairCounts = newPairCounts;
    }

    long[char] elementCounts;
    foreach (pair, count; pairCounts) {
        elementCounts[pair[0]] += count;
    }
    elementCounts[template_[$ - 1]]++;

    auto counts = elementCounts.values;
    writeln(counts.maxElement - counts.minElement);
}
