
import std.stdio;
import std.conv;
import std.array;
import std.algorithm;
import std.string;
import std.file;

int[][] parseInput(string[] input) {
    int[][] histories;

    foreach (line; input) {
        int[] numbers = line.splitter.map!(a => to!int(a)).array;
        histories ~= numbers;
    }

    return histories;
}

bool allZeros(int[] nums) {
    return !nums.any!(a => a != 0);
}

int[] calculateExtrapolation(int[] history) {
    int[] extrapolations;

    foreach (i; 1 .. history.length) {
        extrapolations ~= history[i] - history[i - 1];
    }

    return extrapolations;
}

int[][] calculateExtrapolations(int[] history) {
    int[][] extrapolationsSeries;
    extrapolationsSeries ~= history;

    foreach (i; 1 .. history.length) {
        int[] previousExtrapolations = extrapolationsSeries[$ - 1];
        if (allZeros(previousExtrapolations)) {
            return extrapolationsSeries;
        }

        int[] extrapolations = calculateExtrapolation(previousExtrapolations);
        extrapolationsSeries ~= extrapolations;
    }

    return extrapolationsSeries;
}

int solve(string[] input) {
    int[][] histories = parseInput(input);
    int res = 0;

    foreach (history; histories) {
        int[][] extrapolationsSeries = calculateExtrapolations(history);

        int futurePrediction = 0;
        foreach_reverse (i; 0 .. extrapolationsSeries.length) {
            futurePrediction = extrapolationsSeries[i][$ - 1] + futurePrediction;
        }

        res += futurePrediction;
    }

    return res;
}

string[] readFile(string fileName) {
    string file = cast(string)std.file.read(fileName);
    return file.strip().splitter("\n").array;
}

void main() {
    string[] input = readFile("input.txt");
    writeln(solve(input));
}
