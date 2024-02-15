
import std.stdio;
import std.conv;
import std.file;

int processLine(string line) {
    return to!int(line);
}

int calcFuelMass(int mass) {
    int fuel = cast(int)((mass / 3) - 2);
    if (fuel <= 0) {
        return 0;
    }
    return fuel + calcFuelMass(fuel);
}

int getTotal(int[] masses) {
    int total = 0;
    foreach (mass; masses) {
        total += calcFuelMass(mass);
    }
    return total;
}

void main() {
    int[] masses;
    foreach (line; File("input.txt").byLine()) {
        masses ~= processLine(line.idup); // Use idup to create a new copy of the line
    }
    int total = getTotal(masses);
    writeln(total);
}
