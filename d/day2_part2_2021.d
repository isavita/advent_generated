
import std.file;
import std.conv;
import std.string;
import std.stdio; // Added import statement for writeln

void main() {
    string input = cast(string) read("input.txt");
    int horizontal = 0;
    int depth = 0;
    int aim = 0;

    foreach (line; input.splitLines()) {
        auto parts = line.split(" ");
        int value = to!int(parts[1]);

        if (parts[0] == "forward") {
            horizontal += value;
            depth += aim * value;
        } else if (parts[0] == "down") {
            aim += value;
        } else if (parts[0] == "up") {
            aim -= value;
        }
    }

    writeln(horizontal * depth);
}
