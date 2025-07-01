
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.math;

void main() {
    long x = 1;
    int cycle = 0;

    auto tick = {
        if (abs(cycle % 40 - x) <= 1) {
            write("#");
        } else {
            write(".");
        }
        
        cycle++;
        
        if (cycle % 40 == 0) {
            writeln();
        }
    };

    foreach (line; readText("input.txt").splitLines()) {
        if (line.empty) continue;

        if (line == "noop") {
            tick();
        } else {
            tick();
            tick();
            x += to!int(line[5 .. $]);
        }
    }
}
