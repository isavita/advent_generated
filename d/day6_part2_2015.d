import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

void main()
{
    auto file = File("input.txt", "r");
    auto lines = file.byLineCopy.array;
    int[1000][1000] grid = 0;

    foreach (line; lines)
    {
        auto parts = line.split(" ");
        int x1, y1, x2, y2;

        if (parts[0] == "toggle")
        {
            auto start = parts[1].split(",");
            auto end = parts[3].split(",");
            x1 = start[0].to!int;
            y1 = start[1].to!int;
            x2 = end[0].to!int;
            y2 = end[1].to!int;

            foreach (x; x1 .. x2 + 1)
            {
                foreach (y; y1 .. y2 + 1)
                {
                    grid[x][y] += 2;
                }
            }
        }
        else
        {
            auto start = parts[2].split(",");
            auto end = parts[4].split(",");
            x1 = start[0].to!int;
            y1 = start[1].to!int;
            x2 = end[0].to!int;
            y2 = end[1].to!int;

            if (parts[1] == "on")
            {
                foreach (x; x1 .. x2 + 1)
                {
                    foreach (y; y1 .. y2 + 1)
                    {
                        grid[x][y] += 1;
                    }
                }
            }
            else if (parts[1] == "off")
            {
                foreach (x; x1 .. x2 + 1)
                {
                    foreach (y; y1 .. y2 + 1)
                    {
                        if (grid[x][y] > 0)
                        {
                            grid[x][y] -= 1;
                        }
                    }
                }
            }
        }
    }

    int totalBrightness = 0;
    foreach (x; 0 .. 1000)
    {
        foreach (y; 0 .. 1000)
        {
            totalBrightness += grid[x][y];
        }
    }

    writeln(totalBrightness);
}