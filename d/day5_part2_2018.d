
import std.stdio;
import std.file;
import std.string;

string react(string polymer) {
    bool reactionOccurred = true;
    while (reactionOccurred) {
        reactionOccurred = false;
        for (size_t i = 0; i < polymer.length - 1; i++) {
            if (polymer[i] != polymer[i+1] && toUpper(polymer[i]) == toUpper(polymer[i+1])) {
                polymer = polymer[0 .. i] ~ polymer[i+2 .. $];
                reactionOccurred = true;
            }
        }
    }
    return polymer;
}

void main() {
    string content;
    try {
        content = cast(string)read("input.txt");
    } catch (Exception e) {
        writeln("Error reading file: ", e.msg);
        return;
    }

    string polymer = content.strip();

    size_t minLength = polymer.length;
    foreach_reverse (unit; 'a' .. 'z') {
        string tempPolymer = polymer.replace(cast(char)unit, "")
                                     .replace(cast(char)toUpper(unit), "");
        string reactedPolymer = react(tempPolymer);
        if (reactedPolymer.length < minLength) {
            minLength = reactedPolymer.length;
        }
    }

    writeln(minLength);
}
