
import std.stdio;
import std.file;
import std.conv;
import std.string;

void main() {
    auto target = readText("input.txt").strip.to!int / 10;
    auto houses = new int[target + 1];

    for (int elf = 1; elf <= target; ++elf) {
        for (int house = elf; house <= target; house += elf) {
            houses[house] += elf;
        }
    }

    foreach (int i, presents; houses) {
        if (presents >= target) {
            writeln(i);
            return;
        }
    }
}
