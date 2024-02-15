
import std.stdio;
import std.file;
import std.conv;
import std.string;

struct Position {
    int x, y;
    int dirIndex;
}

void main() {
    string[] instructions;
    foreach (line; File("input.txt").byLine)
        instructions = line.idup.split(", "); // Added .idup to create a copy of the string

    Position pos = Position(0, 0, 0);
    int[][] directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];

    foreach (instruction; instructions) {
        char turn = instruction[0];
        int blocks = to!int(instruction[1 .. $]);

        if (turn == 'R')
            pos.dirIndex = (pos.dirIndex + 1) % 4;
        else
            pos.dirIndex = (pos.dirIndex - 1 + 4) % 4;

        pos.x += directions[pos.dirIndex][0] * blocks;
        pos.y += directions[pos.dirIndex][1] * blocks;
    }

    writeln(abs(pos.x) + abs(pos.y));
}

int abs(int x) {
    return x < 0 ? -x : x;
}
