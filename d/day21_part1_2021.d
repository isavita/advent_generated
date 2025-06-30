
import std.stdio;
import std.file;
import std.string;
import std.conv;

void main() {
    auto lines = readText("input.txt").splitLines();
    auto pos = [lines[0][28..$].to!int, lines[1][28..$].to!int];
    auto score = [0L, 0L];

    long rollCount = 0;
    long die = 1;
    int turn = 0;

    while (true) {
        int player = turn % 2;

        long move = (die % 100) + ((die + 1) % 100) + ((die + 2) % 100);
        die += 3;
        rollCount += 3;

        pos[player] = (pos[player] + move - 1) % 10 + 1;
        score[player] += pos[player];

        if (score[player] >= 1000) {
            writeln(score[1 - player] * rollCount);
            break;
        }

        turn++;
    }
}
