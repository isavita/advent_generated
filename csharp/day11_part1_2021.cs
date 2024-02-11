
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        int[][] grid = ReadInput("input.txt");

        int totalFlashes = 0;
        for (int step = 0; step < 100; step++)
        {
            totalFlashes += SimulateStep(grid);
        }

        Console.WriteLine(totalFlashes);
    }

    static int[][] ReadInput(string filename)
    {
        string[] lines = File.ReadAllLines(filename);
        return lines.Select(line => line.Select(c => int.Parse(c.ToString())).ToArray()).ToArray();
    }

    static int SimulateStep(int[][] grid)
    {
        int flashes = 0;
        var flashed = new bool[grid.Length, grid[0].Length];

        for (int y = 0; y < grid.Length; y++)
        {
            for (int x = 0; x < grid[y].Length; x++)
            {
                grid[y][x]++;
            }
        }

        for (int y = 0; y < grid.Length; y++)
        {
            for (int x = 0; x < grid[y].Length; x++)
            {
                if (grid[y][x] > 9)
                {
                    flashes += Flash(grid, x, y, flashed);
                }
            }
        }

        for (int y = 0; y < grid.Length; y++)
        {
            for (int x = 0; x < grid[y].Length; x++)
            {
                if (flashed[y, x])
                {
                    grid[y][x] = 0;
                }
            }
        }

        return flashes;
    }

    static int Flash(int[][] grid, int x, int y, bool[,] flashed)
    {
        if (flashed[y, x])
        {
            return 0;
        }

        flashed[y, x] = true;
        int flashes = 1;
        int[][] directions = { new[] { -1, -1 }, new[] { -1, 0 }, new[] { -1, 1 }, new[] { 0, -1 }, new[] { 0, 1 }, new[] { 1, -1 }, new[] { 1, 0 }, new[] { 1, 1 } };

        foreach (var dir in directions)
        {
            int newX = x + dir[0];
            int newY = y + dir[1];
            if (newX >= 0 && newX < grid[0].Length && newY >= 0 && newY < grid.Length)
            {
                grid[newY][newX]++;
                if (grid[newY][newX] > 9)
                {
                    flashes += Flash(grid, newX, newY, flashed);
                }
            }
        }

        return flashes;
    }
}
