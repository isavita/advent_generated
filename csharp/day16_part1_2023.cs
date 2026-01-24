
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var grid = new char[lines.Length][];
        for (int i = 0; i < lines.Length; i++)
            grid[i] = lines[i].ToCharArray();

        var visited = new HashSet<(int x, int y, int dx, int dy)>();
        var todo = new Stack<(int x, int y, int dx, int dy)>();
        todo.Push((0, 0, 1, 0));

        var energized = new HashSet<(int x, int y)>();

        while (todo.Count > 0)
        {
            var (x, y, dx, dy) = todo.Pop();
            if (x < 0 || y < 0 || y >= grid.Length || x >= grid[y].Length) continue;
            var key = (x, y, dx, dy);
            if (visited.Contains(key)) continue;
            visited.Add(key);
            energized.Add((x, y));

            char c = grid[y][x];
            if (c == '.')
                todo.Push((x + dx, y + dy, dx, dy));
            else if (c == '/')
                todo.Push((x - dy, y - dx, -dy, -dx));
            else if (c == '\\')
                todo.Push((x + dy, y + dx, dy, dx));
            else if (c == '|')
            {
                if (dx != 0)
                {
                    todo.Push((x, y + 1, 0, 1));
                    todo.Push((x, y - 1, 0, -1));
                }
                else
                    todo.Push((x + dx, y + dy, dx, dy));
            }
            else if (c == '-')
            {
                if (dy != 0)
                {
                    todo.Push((x + 1, y, 1, 0));
                    todo.Push((x - 1, y, -1, 0));
                }
                else
                    todo.Push((x + dx, y + dy, dx, dy));
            }
        }

        Console.WriteLine(energized.Count);
    }
}
