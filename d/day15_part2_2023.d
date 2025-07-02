
import std.stdio, std.file, std.string, std.conv, std.algorithm;

struct Lens {
    string label;
    int focalLength;
}

ubyte hash(in string s) {
    ubyte current = 0;
    foreach (c; s) {
        current += c;
        current *= 17;
    }
    return current;
}

void main() {
    auto steps = readText("input.txt").strip().split(',');
    Lens[][256] boxes;

    foreach (step; steps) {
        auto opIndex = step.indexOfAny("=-");
        auto label = step[0 .. opIndex];
        auto boxIndex = hash(label);

        if (step[opIndex] == '-') {
            boxes[boxIndex] = boxes[boxIndex].remove!(l => l.label == label);
        } else {
            auto focalLength = to!int(step[opIndex + 1 .. $]);
            auto lensIndex = boxes[boxIndex].countUntil!(l => l.label == label);

            if (lensIndex != -1) {
                boxes[boxIndex][lensIndex].focalLength = focalLength;
            } else {
                boxes[boxIndex] ~= Lens(label, focalLength);
            }
        }
    }

    long totalFocusingPower = 0;
    foreach (i, box; boxes) {
        foreach (j, lens; box) {
            totalFocusingPower += (i + 1) * (j + 1) * lens.focalLength;
        }
    }

    writeln(totalFocusingPower);
}
