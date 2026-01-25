
using System;
using System.IO;

public class Program
{
    private const int Size = 71;
    private static readonly int[,] Dirs = { { 1, 0 }, { -1, 0 }, { 0, 1 }, { 0, -1 } };

    public static bool CanReach(bool[,] grid)
    {
        if (grid[0, 0] || grid[Size - 1, Size - 1])
        {
            return false;
        }

        var visited = new bool[Size, Size];
        var queue = new (int, int)[Size * Size];
        int head = 0, tail = 0;

        queue[tail++] = (0, 0);
        visited[0, 0] = true;

        while (head < tail)
        {
            var (x, y) = queue[head++];
            if (x == Size - 1 && y == Size - 1)
            {
                return true;
            }
            for (int i = 0; i < 4; i++)
            {
                int nx = x + Dirs[i, 0];
                int ny = y + Dirs[i, 1];
                if (nx >= 0 && ny >= 0 && nx < Size && ny < Size && !grid[ny, nx] && !visited[ny, nx])
                {
                    visited[ny, nx] = true;
                    queue[tail++] = (nx, ny);
                }
            }
        }
        return false;
    }

    public static void Main()
    {
        try
        {
            var grid = new bool[Size, Size];
            foreach (var line in File.ReadLines("input.txt"))
            {
                var parts = line.Split(',');
                if (parts.Length == 2 && int.TryParse(parts[0], out int x) && int.TryParse(parts[1], out int y))
                {
                    if (x >= 0 && x < Size && y >= 0 && y < Size)
                    {
                        grid[y, x] = true;
                        if (!CanReach(grid))
                        {
                            Console.WriteLine($"{x},{y}");
                            return;
                        }
                    }
                }
            }
            Console.WriteLine("No cutoff found");
        }
        catch (FileNotFoundException)
        {
            Console.WriteLine("Failed to open input.txt");
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
        }
    }
}
