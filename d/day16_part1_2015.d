
import std.stdio;
import std.string;
import std.conv;

void main() {
    immutable int[string] target = [
        "children": 3, "cats": 7, "samoyeds": 2,
        "pomeranians": 3, "akitas": 0, "vizslas": 0,
        "goldfish": 5, "trees": 3, "cars": 2, "perfumes": 1
    ];

    foreach (line; File("input.txt").byLine) {
        auto parts = line.split();
        bool isMatch = true;

        for (size_t i = 2; i < parts.length; i += 2) {
            auto prop = parts[i][0 .. $-1];
            auto value = to!int(parts[i + 1].strip(","));

            if (prop !in target || target[prop] != value) {
                isMatch = false;
                break;
            }
        }

        if (isMatch) {
            writeln(parts[1][0 .. $-1]);
            break;
        }
    }
}
