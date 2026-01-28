using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = new List<string>(File.ReadAllLines("input.txt"));
        int rows = lines.Count;
        int cols = lines[0].Length;
        char[][] grid = new char[rows][];
        for (int i = 0; i < rows; i++) grid[i] = lines[i].ToCharArray();

        long sumGear = 0, sumPart = 0;
        char[][] gearGrid = new char[rows][];
        for (int i = 0; i < rows; i++) gearGrid[i] = (char[])grid[i].Clone();

        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                if (gearGrid[i][j] != '*') continue;
                var adjNumbers = new HashSet<int>();
                for (int x = Math.Max(0, i - 1); x <= Math.Min(rows - 1, i + 1); x++)
                {
                    for (int y = Math.Max(0, j - 1); y <= Math.Min(cols - 1, j + 1); y++)
                    {
                        if (!char.IsDigit(gearGrid[x][y])) continue;
                        int start = y;
                        while (start > 0 && char.IsDigit(gearGrid[x][start - 1])) start--;
                        int end = y;
                        while (end < cols - 1 && char.IsDigit(gearGrid[x][end + 1])) end++;
                        int num = int.Parse(new string(gearGrid[x], start, end - start + 1));
                        adjNumbers.Add(num);
                        for (int k = start; k <= end; k++) gearGrid[x][k] = '.';
                    }
                }
                if (adjNumbers.Count == 2)
                {
                    var e = adjNumbers.GetEnumerator();
                    e.MoveNext(); long a = e.Current;
                    e.MoveNext(); long b = e.Current;
                    sumGear += a * b;
                }
            }
        }

        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                if (!char.IsDigit(grid[i][j])) continue;
                int start = j;
                while (start > 0 && char.IsDigit(grid[i][start - 1])) start--;
                int end = j;
                while (end < cols - 1 && char.IsDigit(grid[i][end + 1])) end++;
                bool isPart = false;
                for (int x = Math.Max(0, i - 1); x <= Math.Min(rows - 1, i + 1) && !isPart; x++)
                {
                    for (int y = Math.Max(0, start - 1); y <= Math.Min(cols - 1, end + 1); y++)
                    {
                        if (!char.IsDigit(grid[x][y]) && grid[x][y] != '.')
                        {
                            isPart = true;
                            break;
                        }
                    }
                }
                if (isPart) sumPart += int.Parse(new string(grid[i], start, end - start + 1));
                j = end;
            }
        }

        Console.WriteLine($"Sum of part numbers: {sumPart}");
        Console.WriteLine($"Sum of gear ratios: {sumGear}");
    }
}