import std.stdio;
import std.file;
import std.array;
import std.conv;

void main()
{
    string input = cast(string)std.file.read("input.txt");
    string[] lines = input.split("\n");

    char[][] grid;
    foreach (line; lines)
    {
        grid ~= line.dup;
    }

    foreach (col; 0 .. grid[0].length)
    {
        int emptyRow = 0;
        foreach (row; 0 .. grid.length)
        {
            if (grid[row][col] == 'O')
            {
                grid[row][col] = '.';
                grid[cast(int)emptyRow][col] = 'O';
                emptyRow++;
            }
            else if (grid[row][col] == '#')
            {
                emptyRow = cast(int)(row + 1);
            }
        }
    }

    int totalLoad = 0;
    foreach (i, row; grid)
    {
        foreach (cell; row)
        {
            if (cell == 'O')
            {
                totalLoad += grid.length - cast(int)i;
            }
        }
    }

    writeln(totalLoad);
}