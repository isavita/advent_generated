
import std.stdio;
import std.file;
import std.conv;
import std.algorithm;
import std.array;
import std.string;

void main() {
    string data = cast(string) read("input.txt");
    auto frequencyChanges = data.splitter("\n").array;
    int[string] frequencies;
    int currentFrequency = 0;
    frequencies[to!string(currentFrequency)] = 1;

    while (true) {
        foreach (change; frequencyChanges) {
            int frequencyDelta = to!int(change);
            currentFrequency += frequencyDelta;
            if (to!string(currentFrequency) in frequencies) {
                writeln(to!string(currentFrequency));
                return;
            }
            frequencies[to!string(currentFrequency)] = 1;
        }
    }
}
