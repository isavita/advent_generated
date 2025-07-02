
import std.stdio, std.file, std.string, std.conv, std.array;

void main() {
    auto steps = readText("input.txt").strip.to!int;
    auto buffer = [0];
    buffer.reserve(2018);
    size_t pos = 0;

    foreach (i; 1 .. 2018) {
        pos = (pos + steps) % buffer.length;
        buffer.insertInPlace(pos + 1, i);
        pos++;
    }

    writeln(buffer[(pos + 1) % buffer.length]);
}
