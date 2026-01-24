
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        int input = int.Parse(File.ReadAllText("input.txt").Trim());

        int layer = 0, size = 1;
        while (size * size < input) { layer++; size += 2; }
        int cycle = size - 1;
        int offset = input - (size - 2) * (size - 2);
        offset %= cycle;
        int steps = layer + Math.Abs(offset - layer);
        Console.WriteLine(steps);

        var map = new Dictionary<(int, int), int> { [(0, 0)] = 1 };
        int x = 0, y = 0, dx = 0, dy = -1, val = 1;
        while (val <= input)
        {
            if ((x == y) || (x < 0 && x == -y) || (x > 0 && x == 1 - y))
            {
                int t = dx; dx = -dy; dy = t;
            }
            x += dx; y += dy;
            int sum = 0;
            for (int i = -1; i <= 1; i++)
                for (int j = -1; j <= 1; j++)
                    if (map.TryGetValue((x + i, y + j), out int v)) sum += v;
            map[(x, y)] = sum;
            val = sum;
        }
        Console.WriteLine(val);
    }
}
