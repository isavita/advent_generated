using System;
using System.IO;

class Program
{
    static int GetCoord(string s)
    {
        int idx = s.IndexOf('+');
        if (idx == -1) idx = s.IndexOf('=');
        return int.Parse(s.Substring(idx + 1));
    }

    static int Solve(int ax, int ay, int bx, int by, int px, int py)
    {
        int best = int.MaxValue;
        for (int a = 0; a <= 100; a++)
        {
            int x = ax * a;
            int y = ay * a;
            if (x > px || y > py) continue;
            for (int b = 0; b <= 100; b++)
            {
                int xx = x + bx * b;
                int yy = y + by * b;
                if (xx == px && yy == py)
                {
                    int cost = a * 3 + b;
                    if (cost < best) best = cost;
                }
            }
        }
        return best == int.MaxValue ? -1 : best;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int solved = 0;
        long total = 0;
        int ax = 0, ay = 0, bx = 0, by = 0, px = 0, py = 0;
        bool hasA = false, hasB = false, hasP = false;

        foreach (var raw in lines)
        {
            var line = raw.Trim();
            if (line.Length == 0)
            {
                if (hasA && hasB && hasP)
                {
                    int cost = Solve(ax, ay, bx, by, px, py);
                    if (cost != -1)
                    {
                        solved++;
                        total += cost;
                    }
                }
                hasA = hasB = hasP = false;
                continue;
            }

            if (line.StartsWith("Button A:") || line.StartsWith("A:"))
            {
                var parts = line.Split(':')[1].Split(',');
                ax = GetCoord(parts[0]);
                ay = GetCoord(parts[1]);
                hasA = true;
            }
            else if (line.StartsWith("Button B:") || line.StartsWith("B:"))
            {
                var parts = line.Split(':')[1].Split(',');
                bx = GetCoord(parts[0]);
                by = GetCoord(parts[1]);
                hasB = true;
            }
            else if (line.StartsWith("Prize:") || line.StartsWith("P:"))
            {
                var parts = line.Split(':')[1].Split(',');
                px = GetCoord(parts[0]);
                py = GetCoord(parts[1]);
                hasP = true;
            }
        }

        if (hasA && hasB && hasP)
        {
            int cost = Solve(ax, ay, bx, by, px, py);
            if (cost != -1)
            {
                solved++;
                total += cost;
            }
        }

        Console.WriteLine($"{solved} {total}");
    }
}