import std.stdio;
import std.file;
import std.conv;
import std.regex;
import std.algorithm; // Added import for startsWith

void main()
{
    auto file = File("input.txt", "r");
    bool[1000][1000] grid;
    foreach (line; file.byLine())
    {
        auto matches = matchFirst(line, r"(\d+),(\d+) through (\d+),(\d+)");
        if (matches.empty)
            continue;

        int x1 = to!int(matches[1]);
        int y1 = to!int(matches[2]);
        int x2 = to!int(matches[3]);
        int y2 = to!int(matches[4]);

        if (line.startsWith("turn on"))
        {
            for (int x = x1; x <= x2; x++)
                for (int y = y1; y <= y2; y++)
                    grid[x][y] = true;
        }
        else if (line.startsWith("turn off"))
        {
            for (int x = x1; x <= x2; x++)
                for (int y = y1; y <= y2; y++)
                    grid[x][y] = false;
        }
        else if (line.startsWith("toggle"))
        {
            for (int x = x1; x <= x2; x++)
                for (int y = y1; y <= y2; y++)
                    grid[x][y] = !grid[x][y];
        }
    }

    int countLit = 0;
    for (int x = 0; x < 1000; x++)
        for (int y = 0; y < 1000; y++)
            if (grid[x][y])
                countLit++;

    writeln(countLit);
}