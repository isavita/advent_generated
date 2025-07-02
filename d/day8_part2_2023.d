
import std.stdio;
import std.file;
import std.string;
import std.numeric;
import std.algorithm;

void main() {
    auto lines = readText("input.txt").strip().splitLines();

    auto instructions = lines[0];
    string[2][string] nodes;
    string[] starts;

    foreach (line; lines[2 .. $]) {
        auto parts = line.split(" = ");
        auto head = parts[0];
        if (head[$-1] == 'A') {
            starts ~= head;
        }
        auto children = parts[1][1 .. $-1].split(", ");
        nodes[head] = [children[0], children[1]];
    }

    long[] steps;
    steps.length = starts.length;

    foreach (i, startNode; starts) {
        auto current = startNode;
        while (current[$-1] != 'Z') {
            auto instruction = instructions[steps[i] % instructions.length];
            current = nodes[current][instruction == 'L' ? 0 : 1];
            steps[i]++;
        }
    }

    writeln(reduce!lcm(1L, steps));
}
