
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;

public class Solution
{
    const char Open = '.';
    const char Trees = '|';
    const char Lumberyard = '#';
    const int Size = 50;

    public static void Main()
    {
        var grid = ReadInput("input.txt");

        Dictionary<string, int> seenStates = new Dictionary<string, int>();
        int cycleStart = 0, cycleLength = 0;
        for (int minute = 0; ; minute++)
        {
            string state = GridToString(grid);
            if (seenStates.ContainsKey(state))
            {
                cycleStart = seenStates[state];
                cycleLength = minute - seenStates[state];
                break;
            }
            seenStates[state] = minute;
            grid = Transform(grid);
        }

        int remainingMinutes = (1000000000 - cycleStart) % cycleLength;
        for (int i = 0; i < remainingMinutes; i++)
        {
            grid = Transform(grid);
        }

        (int wooded, int lumberyards) = CountResources(grid);
        Console.WriteLine(wooded * lumberyards);
    }

    public static char[][] ReadInput(string filename)
    {
        string[] lines = File.ReadAllLines(filename);
        char[][] grid = new char[lines.Length][];
        for (int i = 0; i < lines.Length; i++)
        {
            grid[i] = lines[i].ToCharArray();
        }
        return grid;
    }

    public static char[][] Transform(char[][] grid)
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

    public static char NextAcreState(char[][] grid, int i, int j)
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

    public static int CountAdjacent(char[][] grid, int i, int j, char acreType)
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

    public static (int, int) CountResources(char[][] grid)
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

    public static string GridToString(char[][] grid)
    {
        StringBuilder sb = new StringBuilder();
        foreach (char[] row in grid)
        {
            sb.AppendLine(new string(row));
        }
        return sb.ToString();
    }
}
