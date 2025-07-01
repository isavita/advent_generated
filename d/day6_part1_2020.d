
import std.stdio;
import std.file;
import std.string;
import std.algorithm;

void main() {
    long totalCount = 0;
    foreach (group; readText("input.txt").splitter("\n\n")) {
        bool[char] questions;
        foreach (line; group.splitter('\n')) {
            foreach (c; line) {
                questions[c] = true;
            }
        }
        totalCount += questions.length;
    }
    writeln(totalCount);
}
