
import std.stdio, std.string, std.conv, std.file;

void main() {
    auto b = readText("input.txt").splitLines[0].split[2].to!long;
    writeln((b - 2) * (b - 2));
}
