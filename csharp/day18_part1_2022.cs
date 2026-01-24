
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        var grid = new bool[100, 100, 100];
        foreach (var line in File.ReadAllLines("input.txt"))
        {
            var p = line.Split(',');
            grid[int.Parse(p[0]), int.Parse(p[1]), int.Parse(p[2])] = true;
        }

        int area = 0;
        for (int x = 0; x < 100; x++)
            for (int y = 0; y < 100; y++)
                for (int z = 0; z < 100; z++)
                    if (grid[x, y, z])
                    {
                        area += 6;
                        if (x > 0 && grid[x - 1, y, z]) area--;
                        if (x < 99 && grid[x + 1, y, z]) area--;
                        if (y > 0 && grid[x, y - 1, z]) area--;
                        if (y < 99 && grid[x, y + 1, z]) area--;
                        if (z > 0 && grid[x, y, z - 1]) area--;
                        if (z < 99 && grid[x, y, z + 1]) area--;
                    }

        Console.WriteLine(area);
    }
}
