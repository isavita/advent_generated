
using System;
using System.IO;

class Program
{
    static bool CheckMAS(char[][] grid, int x, int y, int dx, int dy)
    {
        int rows = grid.Length, cols = grid[0].Length;
        if (x < 0 || y < 0 || x >= rows || y >= cols) return false;
        bool f = true, b = true;
        for (int i = 0; i < 3; i++)
        {
            int nx = x + dx * i, ny = y + dy * i;
            if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != "MAS"[i]) f = false;
            if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != "MAS"[2 - i]) b = false;
        }
        return f || b;
    }

    static bool CheckXMAS(char[][] grid, int x, int y)
    {
        return (CheckMAS(grid, x - 1, y - 1, 1, 1) && CheckMAS(grid, x - 1, y + 1, 1, -1)) ||
               (CheckMAS(grid, x + 1, y - 1, -1, 1) && CheckMAS(grid, x + 1, y + 1, -1, -1));
    }

    static int CountXMASPatterns(char[][] grid)
    {
        int rows = grid.Length, cols = grid[0].Length;
        if (rows < 3 || cols < 3) return 0;
        int count = 0;
        for (int i = 1; i < rows - 1; i++)
            for (int j = 1; j < cols - 1; j++)
                if (grid[i][j] == 'A' && CheckXMAS(grid, i, j)) count++;
        return count;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int rows = lines.Length;
        var grid = new char[rows][];
        for (int i = 0; i < rows; i++)
            grid[i] = lines[i].ToCharArray();
        Console.WriteLine(CountXMASPatterns(grid));
    }
}
