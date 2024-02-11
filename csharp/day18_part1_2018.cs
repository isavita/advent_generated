
using System;
using System.IO;

class Program
{
    const char Open = '.';
    const char Trees = '|';
    const char Lumberyard = '#';
    const int Size = 50;

    static void Main()
    {
        char[][] grid = ReadInput("input.txt");

        for (int minute = 0; minute < 10; minute++)
        {
            grid = Transform(grid);
        }

        (int wooded, int lumberyards) = CountResources(grid);
        Console.WriteLine(wooded * lumberyards);
    }

    static char[][] ReadInput(string filename)
    {
        string[] lines = File.ReadAllLines(filename);
        char[][] grid = new char[lines.Length][];
        for (int i = 0; i < lines.Length; i++)
        {
            grid[i] = lines[i].ToCharArray();
        }
        return grid;
    }

    static char[][] Transform(char[][] grid)
    {
        char[][] newGrid = new char[grid.Length][];
        for (int i = 0; i < grid.Length; i++)
        {
            newGrid[i] = new char[grid[i].Length];
            for (int j = 0; j < grid[i].Length; j++)
            {
                newGrid[i][j] = NextAcreState(grid, i, j);
            }
        }
        return newGrid;
    }

    static char NextAcreState(char[][] grid, int i, int j)
    {
        switch (grid[i][j])
        {
            case Open:
                if (CountAdjacent(grid, i, j, Trees) >= 3)
                {
                    return Trees;
                }
                break;
            case Trees:
                if (CountAdjacent(grid, i, j, Lumberyard) >= 3)
                {
                    return Lumberyard;
                }
                break;
            case Lumberyard:
                if (CountAdjacent(grid, i, j, Lumberyard) >= 1 && CountAdjacent(grid, i, j, Trees) >= 1)
                {
                    return Lumberyard;
                }
                return Open;
        }
        return grid[i][j];
    }

    static int CountAdjacent(char[][] grid, int i, int j, char acreType)
    {
        int count = 0;
        for (int x = -1; x <= 1; x++)
        {
            for (int y = -1; y <= 1; y++)
            {
                if (x == 0 && y == 0)
                {
                    continue;
                }
                if (i + x >= 0 && i + x < grid.Length && j + y >= 0 && j + y < grid[i].Length && grid[i + x][j + y] == acreType)
                {
                    count++;
                }
            }
        }
        return count;
    }

    static (int, int) CountResources(char[][] grid)
    {
        int wooded = 0, lumberyards = 0;
        foreach (char[] row in grid)
        {
            foreach (char acre in row)
            {
                if (acre == Trees)
                {
                    wooded++;
                }
                else if (acre == Lumberyard)
                {
                    lumberyards++;
                }
            }
        }
        return (wooded, lumberyards);
    }
}
