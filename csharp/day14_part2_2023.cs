using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static readonly char EMPTY = '.';
    static readonly char CUBIC_ROCK = '#';
    static readonly char ROUND_ROCK = 'O';

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int height = lines.Length;
        int width = lines[0].Length;
        char[][] grid = new char[height][];
        for (int y = 0; y < height; y++)
            grid[y] = lines[y].ToCharArray();

        int totalCycles = 1_000_000_000;
        var cache = new Dictionary<int, int>();
        int cycleIndex = 0;

        while (cycleIndex < totalCycles)
        {
            int key = CalculateGridKey(grid, width, height);
            if (cache.TryGetValue(key, out int firstIndex))
            {
                int cycleLength = cycleIndex - firstIndex;
                int remaining = (int)((totalCycles - cycleIndex) % cycleLength);
                for (int r = 0; r < remaining; r++)
                    CycleRocks(grid, width, height);
                break;
            }
            cache[key] = cycleIndex;
            CycleRocks(grid, width, height);
            cycleIndex++;
        }

        int load = CalculateLoad(grid, width, height);
        Console.WriteLine(load);
    }

    static int CalculateGridKey(char[][] grid, int width, int height)
    {
        int key = 0;
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                if (grid[y][x] == ROUND_ROCK)
                    key += x + y * width;
        return key;
    }

    static int CalculateLoad(char[][] grid, int width, int height)
    {
        int load = 0;
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                if (grid[y][x] == ROUND_ROCK)
                    load += height - y;
        return load;
    }

    static void CycleRocks(char[][] grid, int width, int height)
    {
        ShiftRocks(grid, width, height, 0, -1); // up
        ShiftRocks(grid, width, height, -1, 0); // left
        ShiftRocks(grid, width, height, 0, 1);  // down
        ShiftRocks(grid, width, height, 1, 0);  // right
    }

    static void ShiftRocks(char[][] grid, int width, int height, int dx, int dy)
    {
        if (dy < 0 || dx < 0)
        {
            for (int x = 0; x < width; x++)
                for (int y = 0; y < height; y++)
                    ShiftSingleRock(grid, width, height, x, y, dx, dy);
        }
        else
        {
            for (int x = width - 1; x >= 0; x--)
                for (int y = height - 1; y >= 0; y--)
                    ShiftSingleRock(grid, width, height, x, y, dx, dy);
        }
    }

    static void ShiftSingleRock(char[][] grid, int width, int height, int x, int y, int dx, int dy)
    {
        if (grid[y][x] != ROUND_ROCK) return;
        int cx = x, cy = y;
        while (true)
        {
            int nx = cx + dx, ny = cy + dy;
            if (nx < 0 || nx >= width || ny < 0 || ny >= height || grid[ny][nx] != EMPTY) break;
            grid[ny][nx] = ROUND_ROCK;
            grid[cy][cx] = EMPTY;
            cx = nx; cy = ny;
        }
    }
}