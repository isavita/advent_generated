
import std.stdio;
import std.file;
import std.string;
import std.typecons;

void main() {
    auto lines = readText("input.txt").strip().split("\n");
    auto instructions = lines[0];

    alias Node = Tuple!(string, "left", string, "right");
    Node[string] desertMap;

    foreach (line; lines[2..$]) {
        if (line.length < 16) continue;
        desertMap[line[0..3]] = Node(line[7..10], line[12..15]);
    }

    string current = "AAA";
    long steps = 0;

    while (current != "ZZZ") {
        char instruction = instructions[steps % instructions.length];
        current = (instruction == 'L') ? desertMap[current].left : desertMap[current].right;
        steps++;
    }

    writeln(steps);
}
