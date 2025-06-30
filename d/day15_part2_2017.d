
import std.stdio;
import std.file;
import std.string;
import std.conv;

void main() {
    auto lines = readText("input.txt").strip().split("\n");
    long genA = to!long(lines[0]);
    long genB = to!long(lines[1]);

    enum long genAFactor = 16807;
    enum long genBFactor = 48271;
    enum long modulus = 2147483647;

    int matches = 0;
    foreach (i; 0 .. 5_000_000) {
        do {
            genA = (genA * genAFactor) % modulus;
        } while (genA % 4 != 0);

        do {
            genB = (genB * genBFactor) % modulus;
        } while (genB % 8 != 0);

        if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
            matches++;
        }
    }

    writeln(matches);
}
