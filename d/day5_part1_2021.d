import std.stdio;
import std.algorithm;
import std.conv;
import std.array;
import std.file;

void main()
{
    string input = cast(string)std.file.read("input.txt");
    auto lines = input.split("\n").filter!(a => a.length > 0);

    int[1000][1000] grid = 0;
    int count = 0;

    foreach (line; lines)
    {
        auto parts = line.split(" -> ");
        auto start = parts[0].split(",").map!(to!int).array;
        auto end = parts[1].split(",").map!(to!int).array;

        if (start[0] == end[0])
        {
            foreach (y; min(start[1], end[1]) .. max(start[1], end[1]) + 1)
            {
                grid[start[0]][y]++;
                if (grid[start[0]][y] == 2)
                {
                    count++;
                }
            }
        }
        else if (start[1] == end[1])
        {
            foreach (x; min(start[0], end[0]) .. max(start[0], end[0]) + 1)
            {
                grid[x][start[1]]++;
                if (grid[x][start[1]] == 2)
                {
                    count++;
                }
            }
        }
    }

    writeln(count);
}