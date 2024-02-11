
using System;
using System.IO;
using System.Linq;

class Point
{
    public int X;
    public int Y;
}

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Point[] points = new Point[lines.Length];
        int maxX = 0, maxY = 0;

        for (int i = 0; i < lines.Length; i++)
        {
            string[] coords = lines[i].Split(", ");
            int x = int.Parse(coords[0]);
            int y = int.Parse(coords[1]);
            if (x > maxX)
            {
                maxX = x;
            }
            if (y > maxY)
            {
                maxY = y;
            }
            points[i] = new Point { X = x, Y = y };
        }

        int[][] grid = new int[maxX + 2][];
        for (int i = 0; i < grid.Length; i++)
        {
            grid[i] = new int[maxY + 2];
        }

        int[] areas = new int[points.Length];
        bool[] infinite = new bool[points.Length];

        for (int i = 0; i < grid.Length; i++)
        {
            for (int j = 0; j < grid[i].Length; j++)
            {
                int minDist = maxX + maxY;
                for (int k = 0; k < points.Length; k++)
                {
                    int dist = Math.Abs(points[k].X - i) + Math.Abs(points[k].Y - j);
                    if (dist < minDist)
                    {
                        minDist = dist;
                        grid[i][j] = k;
                    }
                    else if (dist == minDist)
                    {
                        grid[i][j] = -1;
                    }
                }

                if (grid[i][j] != -1)
                {
                    if (i == 0 || j == 0 || i == maxX + 1 || j == maxY + 1)
                    {
                        infinite[grid[i][j]] = true;
                    }
                    areas[grid[i][j]]++;
                }
            }
        }

        int maxArea = 0;
        for (int i = 0; i < areas.Length; i++)
        {
            if (!infinite[i] && areas[i] > maxArea)
            {
                maxArea = areas[i];
            }
        }

        Console.WriteLine(maxArea);
    }
}
