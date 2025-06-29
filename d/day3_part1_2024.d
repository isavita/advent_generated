
import std.stdio;
import std.file;
import std.regex;
import std.conv;
import std.algorithm;
import std.range;

void main() {
    auto re = ctRegex!(`mul\s*\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*\)`);

    auto totalSum = readText("input.txt")
        .matchAll(re)
        .map!(m => to!int(m.captures[1]) * to!int(m.captures[2]))
        .sum();

    writeln(totalSum);
}
