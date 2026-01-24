using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Program
{
    struct Star { public int x, y, vx, vy; }
    static void Main()
    {
        var stars = new List<Star>();
        var rgx = new Regex(@"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>");
        foreach (var line in File.ReadLines("input.txt"))
        {
            var m = rgx.Match(line);
            if (m.Success)
                stars.Add(new Star
                {
                    x = int.Parse(m.Groups[1].Value),
                    y = int.Parse(m.Groups[2].Value),
                    vx = int.Parse(m.Groups[3].Value),
                    vy = int.Parse(m.Groups[4].Value)
                });
        }

        int bestT = 0, bestArea = int.MaxValue;
        for (int t = 1; t < 100000; t++)
        {
            int minX = int.MaxValue, minY = int.MaxValue, maxX = int.MinValue, maxY = int.MinValue;
            foreach (var s in stars)
            {
                int x = s.x + s.vx * t;
                int y = s.y + s.vy * t;
                if (x < minX) minX = x;
                if (x > maxX) maxX = x;
                if (y < minY) minY = y;
                if (y > maxY) maxY = y;
            }
            int area = (maxX - minX + 1) + (maxY - minY + 1);
            if (area < bestArea) { bestArea = area; bestT = t; }
        }

        for (int i = 0; i < stars.Count; i++)
        {
            var s = stars[i];
            s.x += s.vx * bestT;
            s.y += s.vy * bestT;
            stars[i] = s;
        }

        int minXFinal = int.MaxValue, minYFinal = int.MaxValue, maxXFinal = int.MinValue, maxYFinal = int.MinValue;
        foreach (var s in stars)
        {
            if (s.x < minXFinal) minXFinal = s.x;
            if (s.x > maxXFinal) maxXFinal = s.x;
            if (s.y < minYFinal) minYFinal = s.y;
            if (s.y > maxYFinal) maxYFinal = s.y;
        }

        int width = maxXFinal - minXFinal + 1;
        int height = maxYFinal - minYFinal + 1;
        var grid = new char[height, width];
        for (int y = 0; y < height; y++) for (int x = 0; x < width; x++) grid[y, x] = ' ';
        foreach (var s in stars) grid[s.y - minYFinal, s.x - minXFinal] = '#';
        for (int y = 0; y < height; y++)
        {
            var line = new char[width];
            for (int x = 0; x < width; x++) line[x] = grid[y, x];
            Console.WriteLine(line);
        }
    }
}
