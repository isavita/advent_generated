
import std.stdio;
import std.algorithm;
import std.conv;
import std.array;
import std.string;
import std.range;
import std.file;

int calculateNewFuel(int currentPosition, int newPosition) {
    int diff = abs(currentPosition - newPosition);
    return (diff * (diff + 1)) / 2;
}

int abs(int n) {
    if (n < 0) {
        return -n;
    }
    return n;
}

void main() {
    int[] positions;

    string input = cast(string)std.file.read("input.txt");
    foreach (line; input.splitLines) {
        foreach (num_str; line.split(",")) {
            int num = to!int(num_str);
            positions ~= num;
        }
    }

    sort(positions);

    int min_fuel = int.max;
    foreach (i; positions[0] .. positions[$-1]) {
        int fuel = 0;
        foreach (pos; positions) {
            fuel += calculateNewFuel(pos, i);
        }
        if (fuel < min_fuel) {
            min_fuel = fuel;
        }
    }
    writeln(min_fuel);
}
