
import std.stdio;
import std.file;
import std.string;
import std.conv;

void main() {
    auto target = to!int(strip(readText("input.txt"))) / 11;
    auto houses = new uint[target + 1];

    for (uint elf = 1; elf <= target; ++elf) {
        for (size_t i = 1; i <= 50; ++i) {
            auto house = elf * i;
            if (house > target) {
                break;
            }
            houses[house] += elf;
        }
    }

    for (uint houseNumber = 1; houseNumber <= target; ++houseNumber) {
        if (houses[houseNumber] >= target) {
            writeln(houseNumber);
            return;
        }
    }
}
