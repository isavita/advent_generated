import std.stdio;
import std.file;
import std.conv;
import std.regex;

void main()
{
    auto file = File("input.txt", "r");
    int[1000][1000] fabric = 0;

    foreach (line; file.byLine())
    {
        auto matches = line.idup.matchAll(regex(r"#\d+ @ (\d+),(\d+): (\d+)x(\d+)"));
        if (matches.empty)
            continue;

        int left = to!int(matches.front.captures[1]);
        int top = to!int(matches.front.captures[2]);
        int width = to!int(matches.front.captures[3]);
        int height = to!int(matches.front.captures[4]);

        for (int i = left; i < left + width; i++)
        {
            for (int j = top; j < top + height; j++)
            {
                fabric[i][j]++;
            }
        }
    }

    int overlap = 0;
    foreach (i; 0 .. 1000)
    {
        foreach (j; 0 .. 1000)
        {
            if (fabric[i][j] > 1)
            {
                overlap++;
            }
        }
    }

    writeln(overlap);
}