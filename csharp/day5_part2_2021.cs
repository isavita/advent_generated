
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static int Abs(int x)
    {
        return x < 0 ? -x : x;
    }

    static int Sign(int x)
    {
        if (x > 0)
        {
            return 1;
        }
        else if (x < 0)
        {
            return -1;
        }
        return 0;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Dictionary<Tuple<int, int>, int> overlaps = new Dictionary<Tuple<int, int>, int>();

        foreach (string line in lines)
        {
            string[] parts = line.Split(new string[] { " -> " }, StringSplitOptions.None);
            string[] start = parts[0].Split(',');
            string[] end = parts[1].Split(',');

            int x1 = int.Parse(start[0]);
            int y1 = int.Parse(start[1]);
            int x2 = int.Parse(end[0]);
            int y2 = int.Parse(end[1]);

            int xStep = Sign(x2 - x1);
            int yStep = Sign(y2 - y1);
            int steps = Abs(x2 - x1) + 1;
            if (Abs(y2 - y1) > Abs(x2 - x1))
            {
                steps = Abs(y2 - y1) + 1;
            }

            for (int i = 0; i < steps; i++)
            {
                Tuple<int, int> point = new Tuple<int, int>(x1 + i * xStep, y1 + i * yStep);
                if (overlaps.ContainsKey(point))
                {
                    overlaps[point]++;
                }
                else
                {
                    overlaps.Add(point, 1);
                }
            }
        }

        int count = 0;
        foreach (int v in overlaps.Values)
        {
            if (v > 1)
            {
                count++;
            }
        }

        Console.WriteLine(count);
    }
}
