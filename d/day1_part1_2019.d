import std.stdio;
import std.conv;
import std.algorithm;
import std.range;

void main() {
    auto inputFile = File("input.txt", "r");

    int totalFuel = inputFile.byLine()
                              .map!(to!int)
                              .map!(a => a / 3 - 2)
                              .sum;

    writeln(totalFuel); 
}

