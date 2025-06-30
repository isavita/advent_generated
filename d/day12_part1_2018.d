
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.range;

void main() {
    bool[string] rules;
    bool[long] state;

    auto lines = readText("input.txt").split("\n");

    foreach (i, c; lines[0][15..$]) {
        if (c == '#') {
            state[i] = true;
        }
    }

    foreach (line; lines[2..$]) {
        if (line.empty) continue;
        if (line[9] == '#') {
            rules[line[0..5]] = true;
        }
    }

    foreach (generation; 0..20) {
        bool[long] newState;
        if (state.empty) break;

        auto keys = state.keys;
        auto minPot = keys.minElement;
        auto maxPot = keys.maxElement;

        foreach (i; (minPot - 2) .. (maxPot + 3)) {
            char[5] pattern;
            foreach (j; 0..5) {
                pattern[j] = (i - 2 + j) in state ? '#' : '.';
            }
            if (pattern.idup in rules) {
                newState[i] = true;
            }
        }
        state = newState;
    }

    writeln(state.keys.sum);
}
