
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.array;

void main() {
    auto lines = readText("input.txt").strip().splitLines();
    auto copies = new long[lines.length];
    copies[] = 1;

    foreach (i, line; lines) {
        auto numberParts = line.split(':')[1].split('|');
        
        bool[int] winningSet;
        foreach(s; numberParts[0].splitter(' ')) {
            if (!s.empty) {
                winningSet[s.to!int] = true;
            }
        }

        int matches = 0;
        foreach(s; numberParts[1].splitter(' ')) {
            if (!s.empty && s.to!int in winningSet) {
                matches++;
            }
        }

        foreach (j; i + 1 .. min(i + 1 + matches, copies.length)) {
            copies[j] += copies[i];
        }
    }

    writeln(copies.sum);
}
