import std.stdio;
import std.file;
import std.conv;

void main()
{
    int[] offsets;
    foreach (line; File("input.txt").byLine())
    {
        offsets ~= to!int(line);
    }

    int steps = 0;
    int index = 0;

    while (index >= 0 && index < offsets.length)
    {
        int jump = offsets[index];
        offsets[index]++;
        index += jump;
        steps++;
    }

    writeln(steps);
}