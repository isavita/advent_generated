
using System;
using System.IO;

class Program
{
    const int SIZE = 1000;
    static int[,] grid = new int[SIZE, SIZE];

    static void Main()
    {
        foreach (var line in File.ReadLines("input.txt"))
        {
            var p = line.Split(new[] { ',', '-', '>', ' ' }, StringSplitOptions.RemoveEmptyEntries);
            int x1 = int.Parse(p[0]), y1 = int.Parse(p[1]), x2 = int.Parse(p[2]), y2 = int.Parse(p[3]);
            if (x1 == x2)
            {
                if (y1 > y2) (y1, y2) = (y2, y1);
                for (int y = y1; y <= y2; y++) grid[x1, y]++;
            }
            else if (y1 == y2)
            {
                if (x1 > x2) (x1, x2) = (x2, x1);
                for (int x = x1; x <= x2; x++) grid[x, y1]++;
            }
        }

        int overlap = 0;
        for (int i = 0; i < SIZE; i++)
            for (int j = 0; j < SIZE; j++)
                if (grid[i, j] > 1) overlap++;

        Console.WriteLine(overlap);
    }
}
