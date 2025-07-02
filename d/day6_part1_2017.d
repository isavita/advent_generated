
import std.stdio, std.file, std.string, std.conv, std.array, std.algorithm;

void main() {
    auto banks = readText("input.txt").strip.split('\t').map!(to!int).array;

    int cycles = 0;
    bool[immutable(int)[]] seen;

    while (true) {
        auto key = banks.idup;
        if (key in seen) {
            writeln(cycles);
            break;
        }
        seen[key] = true;

        size_t maxIndex = 0;
        foreach (i; 1 .. banks.length) {
            if (banks[i] > banks[maxIndex]) {
                maxIndex = i;
            }
        }

        int blocks = banks[maxIndex];
        banks[maxIndex] = 0;

        for (int i = 0; i < blocks; i++) {
            maxIndex = (maxIndex + 1) % banks.length;
            banks[maxIndex]++;
        }

        cycles++;
    }
}
