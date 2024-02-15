import std.stdio;
import std.file;
import std.conv;
import std.algorithm;
import std.array;

void main()
{
    auto file = File("input.txt", "r");
    int[] adapters = [0];

    foreach (line; file.byLine())
    {
        adapters ~= to!int(line);
    }

    sort(adapters);
    adapters ~= adapters[$ - 1] + 3;

    writeln(countArrangements(adapters));
}

ulong countArrangements(int[] adapters)
{
    ulong[int] ways;
    ways[0] = 1;

    foreach (i, currentJoltage; adapters[1 .. $])
    {
        foreach (diff; [1, 2, 3])
        {
            ways[currentJoltage] += ways.get(currentJoltage - diff, 0);
        }
    }

    return ways[adapters[$ - 1]];
}