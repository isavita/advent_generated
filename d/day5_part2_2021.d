import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;
import std.algorithm;

int abs(int x)
{
    return x < 0 ? -x : x;
}

void main()
{
    string input = cast(string) read("input.txt");
    auto lines = input.split("\n");
    
    int[1000][1000] grid = 0;
    
    foreach (line; lines)
    {
        auto parts = line.split(" -> ");
        auto start = parts[0].split(",");
        auto end = parts[1].split(",");
        
        int x1 = to!int(start[0]);
        int y1 = to!int(start[1]);
        int x2 = to!int(end[0]);
        int y2 = to!int(end[1]);
        
        int dx = x2 - x1;
        int dy = y2 - y1;
        
        int stepX = dx == 0 ? 0 : dx / abs(dx);
        int stepY = dy == 0 ? 0 : dy / abs(dy);
        
        int x = x1;
        int y = y1;
        
        while (x != x2 || y != y2)
        {
            grid[x][y]++;
            x += stepX;
            y += stepY;
        }
        
        grid[x][y]++;
    }
    
    int overlapCount = 0;
    foreach (i; 0 .. 1000)
    {
        foreach (j; 0 .. 1000)
        {
            if (grid[i][j] >= 2)
            {
                overlapCount++;
            }
        }
    }
    
    writeln(overlapCount);
}