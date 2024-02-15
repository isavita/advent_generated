
import std.stdio;
import std.file;
import std.string;
import std.conv;

void main()
{
    string data = cast(string)read("input.txt");
    int serial = data.strip.to!int;

    int gridSize = 300;
    int[][] grid;
    grid.length = gridSize;
    foreach (ref row; grid)
    {
        row.length = gridSize;
    }

    foreach (y; 0 .. gridSize)
    {
        foreach (x; 0 .. gridSize)
        {
            int rackID = x + 11;
            int powerLevel = rackID * (y + 1);
            powerLevel += serial;
            powerLevel *= rackID;
            powerLevel = (powerLevel / 100) % 10;
            powerLevel -= 5;
            grid[y][x] = powerLevel;
        }
    }

    int maxPower = -1 << 31;
    int maxX, maxY;
    foreach (y; 0 .. gridSize-2)
    {
        foreach (x; 0 .. gridSize-2)
        {
            int totalPower = 0;
            foreach (dy; 0 .. 3)
            {
                foreach (dx; 0 .. 3)
                {
                    totalPower += grid[y+dy][x+dx];
                }
            }
            if (totalPower > maxPower)
            {
                maxPower = totalPower;
                maxX = x + 1;
                maxY = y + 1;
            }
        }
    }

    writeln(maxX, ",", maxY);
}
