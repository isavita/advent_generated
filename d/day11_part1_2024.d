
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;

string trimLeadingZeros(string s) {
    if (s.length <= 1) return s;
    auto r = s.stripLeft('0');
    return r.empty ? "0" : r;
}

void main() {
    auto stones = readText("input.txt").strip().split();
    string[] next;

    foreach (_; 0 .. 25) {
        next.length = 0;
        next.reserve(stones.length);

        foreach (s; stones) {
            if (s == "0") {
                next ~= "1";
            } else if (s.length % 2 == 0) {
                auto mid = s.length / 2;
                next ~= trimLeadingZeros(s[0 .. mid]);
                next ~= trimLeadingZeros(s[mid .. $]);
            } else {
                next ~= (s.to!long * 2024).to!string;
            }
        }
        swap(stones, next);
    }

    writeln(stones.length);
}
