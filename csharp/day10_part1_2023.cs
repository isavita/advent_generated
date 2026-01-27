
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var grid = lines.Select(l => l.ToCharArray()).ToArray();
        var start = FindStart(grid);
        var loop = new List<(int r, int c)>();
        var current = start;
        (int r, int c)? prev = null;
        var rows = grid.Length;
        var cols = grid[0].Length;

        do
        {
            loop.Add(current);
            var next = GetNext(current, prev, grid, rows, cols, start);
            if (next == null) break;
            prev = current;
            current = next.Value;
        } while (!current.Equals(start));

        Console.WriteLine(loop.Count / 2);
    }

    static (int r, int c) FindStart(char[][] grid)
    {
        for (int r = 0; r < grid.Length; r++)
            for (int c = 0; c < grid[r].Length; c++)
                if (grid[r][c] == 'S')
                    return (r, c);
        throw new Exception("No start");
    }

    static (int r, int c)? GetNext((int r, int c) pos, (int r, int c)? prev,
        char[][] grid, int rows, int cols, (int r, int c) start)
    {
        var (r, c) = pos;
        var ch = grid[r][c];
        List<(int, int)> candidates = null;

        switch (ch)
        {
            case 'S':
                candidates = new List<(int, int)>();
                if (r > 0 && ("|7F".Contains(grid[r - 1][c]))) candidates.Add((r - 1, c));
                if (r < rows - 1 && ("|LJ".Contains(grid[r + 1][c]))) candidates.Add((r + 1, c));
                if (c > 0 && ("-LF".Contains(grid[r][c - 1]))) candidates.Add((r, c - 1));
                if (c < cols - 1 && ("-J7".Contains(grid[r][c + 1]))) candidates.Add((r, c + 1));
                break;
            case '|':
                candidates = new List<(int, int)>();
                if (r > 0) candidates.Add((r - 1, c));
                if (r < rows - 1) candidates.Add((r + 1, c));
                break;
            case '-':
                candidates = new List<(int, int)>();
                if (c > 0) candidates.Add((r, c - 1));
                if (c < cols - 1) candidates.Add((r, c + 1));
                break;
            case 'L':
                candidates = new List<(int, int)>();
                if (r > 0) candidates.Add((r - 1, c));
                if (c < cols - 1) candidates.Add((r, c + 1));
                break;
            case 'J':
                candidates = new List<(int, int)>();
                if (r > 0) candidates.Add((r - 1, c));
                if (c > 0) candidates.Add((r, c - 1));
                break;
            case '7':
                candidates = new List<(int, int)>();
                if (r < rows - 1) candidates.Add((r + 1, c));
                if (c > 0) candidates.Add((r, c - 1));
                break;
            case 'F':
                candidates = new List<(int, int)>();
                if (r < rows - 1) candidates.Add((r + 1, c));
                if (c < cols - 1) candidates.Add((r, c + 1));
                break;
            default:
                return null;
        }

        foreach (var cand in candidates)
            if (!prev.HasValue || !cand.Equals(prev.Value))
                return cand;
        return null;
    }
}
