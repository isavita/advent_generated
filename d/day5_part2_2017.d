import std.stdio;
import std.file;
import std.conv;

void main()
{
    int[] jumps;
    foreach (line; File("input.txt").byLine())
    {
        jumps ~= to!int(line);
    }

    int currentIndex = 0;
    int steps = 0;

    while (currentIndex >= 0 && currentIndex < jumps.length)
    {
        int offset = jumps[currentIndex];
        jumps[currentIndex] += offset >= 3 ? -1 : 1;
        currentIndex += offset;
        steps++;
    }

    writeln(steps);
}