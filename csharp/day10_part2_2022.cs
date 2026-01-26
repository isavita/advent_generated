
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var xValues = new System.Collections.Generic.List<int> { 1 };
        foreach (var line in File.ReadLines("input.txt"))
        {
            if (line == "noop")
            {
                xValues.Add(xValues.Last());
            }
            else
            {
                var n = int.Parse(line.Split(' ')[1]);
                xValues.Add(xValues.Last());
                xValues.Add(xValues.Last() + n);
            }
        }

        var grid = new bool[6, 40];
        for (int i = 0; i < xValues.Count; i++)
        {
            int crtX = i % 40;
            int crtY = i / 40;
            if (crtY < 6 && Math.Abs(crtX - xValues[i]) <= 1)
            {
                grid[crtY, crtX] = true;
            }
        }

        for (int y = 0; y < 6; y++)
        {
            for (int col = 0; col < 40; col++)
            {
                Console.Write(grid[y, col] ? "#" : ".");
            }
            Console.WriteLine();
        }
    }
}
