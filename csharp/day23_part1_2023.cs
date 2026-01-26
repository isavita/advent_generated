using System;
using System.IO;
using System.Collections.Generic;

class LongestHike
{
    static int maxSteps = 0;

    static void Main()
    {
        var lines = new List<string>();
        foreach (var l in File.ReadLines("input.txt")) lines.Add(l);
        int rows = lines.Count, cols = lines[0].Length;
        var grid = new char[rows, cols];
        for (int i = 0; i < rows; i++)
            for (int j = 0; j < cols; j++)
                grid[i, j] = lines[i][j];
        int startX = 0;
        for (int i = 0; i < cols; i++)
            if (grid[0, i] == '.')
            {
                startX = i;
                break;
            }
        var visited = new bool[rows, cols];
        Dfs(grid, 0, startX, 0, visited);
        Console.WriteLine(maxSteps);
    }

    static void Dfs(char[,] g, int r, int c, int steps, bool[,] v)
    {
        int rows = g.GetLength(0), cols = g.GetLength(1);
        if (r < 0 || r >= rows || c < 0 || c >= cols || g[r, c] == '#' || v[r, c]) return;
        if (r == rows - 1) { if (steps > maxSteps) maxSteps = steps; return; }
        v[r, c] = true;
        switch (g[r, c])
        {
            case '.':
                Dfs(g, r + 1, c, steps + 1, v);
                Dfs(g, r - 1, c, steps + 1, v);
                Dfs(g, r, c + 1, steps + 1, v);
                Dfs(g, r, c - 1, steps + 1, v);
                break;
            case 'v':
                Dfs(g, r + 1, c, steps + 1, v);
                break;
            case '^':
                Dfs(g, r - 1, c, steps + 1, v);
                break;
            case '>':
                Dfs(g, r, c + 1, steps + 1, v);
                break;
            case '<':
                Dfs(g, r, c - 1, steps + 1, v);
                break;
        }
        v[r, c] = false;
    }
}
