
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.typecons;

alias State = Tuple!(int, int, int, int, bool);
alias Memo = long[2][State];

long[2] play(int p1, int s1, int p2, int s2, bool isP1Turn, ref Memo memo) {
    if (s1 >= 21) return [1, 0];
    if (s2 >= 21) return [0, 1];

    auto key = State(p1, s1, p2, s2, isP1Turn);
    if (auto p = key in memo) {
        return *p;
    }

    long[2] totalWins = [0, 0];
    immutable int[2][] rollOutcomes = [
        [3, 1], [4, 3], [5, 6], [6, 7], [7, 6], [8, 3], [9, 1]
    ];

    if (isP1Turn) {
        foreach (outcome; rollOutcomes) {
            auto roll = outcome[0];
            auto freq = outcome[1];
            auto newP1 = (p1 + roll - 1) % 10 + 1;
            auto newS1 = s1 + newP1;
            auto subWins = play(newP1, newS1, p2, s2, false, memo);
            totalWins[0] += subWins[0] * freq;
            totalWins[1] += subWins[1] * freq;
        }
    } else {
        foreach (outcome; rollOutcomes) {
            auto roll = outcome[0];
            auto freq = outcome[1];
            auto newP2 = (p2 + roll - 1) % 10 + 1;
            auto newS2 = s2 + newP2;
            auto subWins = play(p1, s1, newP2, newS2, true, memo);
            totalWins[0] += subWins[0] * freq;
            totalWins[1] += subWins[1] * freq;
        }
    }

    memo[key] = totalWins;
    return totalWins;
}

void main() {
    auto lines = readText("input.txt").strip.splitLines;
    auto p1 = lines[0][lines[0].lastIndexOf(' ') + 1 .. $].to!int;
    auto p2 = lines[1][lines[1].lastIndexOf(' ') + 1 .. $].to!int;

    Memo memo;
    auto wins = play(p1, 0, p2, 0, true, memo);
    
    writeln(max(wins[0], wins[1]));
}
