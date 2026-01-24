using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static int Manhattan(int x1, int y1, int x2, int y2) => Math.Abs(x1 - x2) + Math.Abs(y1 - y2);
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var xs = new List<int>();
        var ys = new List<int>();
        int minX = int.MaxValue, maxX = int.MinValue, minY = int.MaxValue, maxY = int.MinValue;
        foreach (var line in lines)
        {
            var parts = line.Split(',');
            int x = int.Parse(parts[0].Trim());
            int y = int.Parse(parts[1].Trim());
            xs.Add(x);
            ys.Add(y);
            if (x < minX) minX = x;
            if (x > maxX) maxX = x;
            if (y < minY) minY = y;
            if (y > maxY) maxY = y;
        }

        int threshold = 10000;
        int safe = 0;
        for (int i = minX; i <= maxX; i++)
        {
            for (int j = minY; j <= maxY; j++)
            {
                int total = 0;
                for (int k = 0; k < xs.Count; k++)
                {
                    total += Manhattan(i, j, xs[k], ys[k]);
                    if (total >= threshold) break;
                }
                if (total < threshold) safe++;
            }
        }
        Console.WriteLine(safe);
    }
}