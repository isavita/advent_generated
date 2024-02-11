
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[][] heightmap = new int[lines.Length][];

        for (int y = 0; y < lines.Length; y++)
        {
            string line = lines[y];
            heightmap[y] = new int[line.Length];
            for (int x = 0; x < line.Length; x++)
            {
                heightmap[y][x] = int.Parse(line[x].ToString());
            }
        }

        int totalRiskLevel = 0;
        for (int y = 0; y < heightmap.Length; y++)
        {
            for (int x = 0; x < heightmap[y].Length; x++)
            {
                if (IsLowPoint(heightmap, x, y))
                {
                    totalRiskLevel += 1 + heightmap[y][x];
                }
            }
        }

        Console.WriteLine(totalRiskLevel);
    }

    static bool IsLowPoint(int[][] heightmap, int x, int y)
    {
        int height = heightmap[y][x];
        if (x > 0 && heightmap[y][x - 1] <= height)
        {
            return false;
        }
        if (x < heightmap[y].Length - 1 && heightmap[y][x + 1] <= height)
        {
            return false;
        }
        if (y > 0 && heightmap[y - 1][x] <= height)
        {
            return false;
        }
        if (y < heightmap.Length - 1 && heightmap[y + 1][x] <= height)
        {
            return false;
        }
        return true;
    }
}
