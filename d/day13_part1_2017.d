import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.stdio; // Added import statement for writeln

void main()
{
    string input = cast(string) read("input.txt");
    auto lines = input.split("\n");

    int severity = 0;

    foreach (line; lines)
    {
        auto parts = line.split(": ");
        int depth = to!int(parts[0]);
        int range = to!int(parts[1]);

        if (depth % ((range - 1) * 2) == 0)
        {
            severity += depth * range;
        }
    }

    writeln(severity);
}