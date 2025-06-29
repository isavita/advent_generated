
import std.stdio;
import std.string;
import std.conv;

void main() {
    auto file = File("input.txt", "r");
    auto line = file.readln().strip();
    auto startingNumbers = line.split(",");

    auto spoken = new int[30000000];
    int lastSpoken = 0;
    int turn = 0;

    foreach (i, numberStr; startingNumbers) {
        turn++;
        auto num = to!int(numberStr);
        if (i < startingNumbers.length - 1) {
            spoken[num] = turn;
        }
        lastSpoken = num;
    }

    while (turn < 30000000) {
        turn++;
        int nextNumber;
        if (spoken[lastSpoken] != 0) {
            nextNumber = turn - 1 - spoken[lastSpoken];
        } else {
            nextNumber = 0;
        }
        spoken[lastSpoken] = turn - 1;
        lastSpoken = nextNumber;
    }

    writeln(lastSpoken);
}
