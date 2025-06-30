
import std.stdio;
import std.conv;
import std.string;

void main() {
    auto file = File("input.txt", "r");
    long genA = file.readln().strip().to!long;
    long genB = file.readln().strip().to!long;

    enum long genAFactor = 16807;
    enum long genBFactor = 48271;
    enum long modulus = 2147483647;

    int matches = 0;

    foreach (i; 0 .. 40_000_000) {
        genA = (genA * genAFactor) % modulus;
        genB = (genB * genBFactor) % modulus;

        if ((genA & 0xFFFF) == (genB & 0xFFFF)) {
            matches++;
        }
    }

    writeln(matches);
}
