
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[][] heightmap = lines.Select(line => line.Select(c => int.Parse(c.ToString())).ToArray()).ToArray();

        int[] basinSizes = new int[0];
        var visited = new bool[heightmap.Length, heightmap[0].Length];

        for (int y = 0; y < heightmap.Length; y++)
        {
            for (int x = 0; x < heightmap[y].Length; x++)
            {
                if (IsLowPoint(heightmap, x, y))
                {
                    int size = ExploreBasin(heightmap, x, y, visited);
                    Array.Resize(ref basinSizes, basinSizes.Length + 1);
                    basinSizes[basinSizes.Length - 1] = size;
                }
            }
        }

        Array.Sort(basinSizes);
        int result = basinSizes[basinSizes.Length - 1] * basinSizes[basinSizes.Length - 2] * basinSizes[basinSizes.Length - 3];
        Console.WriteLine(result);
    }

    static bool IsLowPoint(int[][] heightmap, int x, int y)
    {
        int height = heightmap[y][x];
        if (x > 0 && heightmap[y][x - 1] <= height)
            return false;
        if (x < heightmap[y].Length - 1 && heightmap[y][x + 1] <= height)
            return false;
        if (y > 0 && heightmap[y - 1][x] <= height)
            return false;
        if (y < heightmap.Length - 1 && heightmap[y + 1][x] <= height)
            return false;
        return true;
    }

    static int ExploreBasin(int[][] heightmap, int x, int y, bool[,] visited)
    {
        if (visited[y, x] || heightmap[y][x] == 9)
            return 0;
        visited[y, x] = true;
        int size = 1;

        int[][] directions = { new[] { 0, -1 }, new[] { -1, 0 }, new[] { 0, 1 }, new[] { 1, 0 } };
        foreach (var dir in directions)
        {
            int newX = x + dir[0];
            int newY = y + dir[1];
            if (newX >= 0 && newX < heightmap[0].Length && newY >= 0 && newY < heightmap.Length)
            {
                size += ExploreBasin(heightmap, newX, newY, visited);
            }
        }
        return size;
    }
}
