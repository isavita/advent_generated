using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    const long OFFSET = 10000000000000L;

    static void Main()
    {
        long totalCost = 0;
        long solved = 0;
        bool reading = false;
        long ax = 0, ay = 0, bx = 0, by = 0, px = 0, py = 0;

        try
        {
            foreach (var lineRaw in File.ReadLines("input.txt"))
            {
                var line = lineRaw.Trim();
                if (line.Length == 0)
                {
                    if (reading)
                    {
                        var cost = Solve(ax, ay, bx, by, px + OFFSET, py + OFFSET);
                        if (cost >= 0) { solved++; totalCost += cost; }
                        ax = ay = bx = by = px = py = 0;
                        reading = false;
                    }
                }
                else
                {
                    reading = true;
                    var lower = line.ToLowerInvariant();
                    if (lower.StartsWith("button a:") || lower.StartsWith("a:"))
                        ParseCoords(line, ref ax, ref ay);
                    else if (lower.StartsWith("button b:") || lower.StartsWith("b:"))
                        ParseCoords(line, ref bx, ref by);
                    else if (lower.StartsWith("prize:") || lower.StartsWith("p:"))
                        ParseCoords(line, ref px, ref py);
                }
            }

            if (reading)
            {
                var cost = Solve(ax, ay, bx, by, px + OFFSET, py + OFFSET);
                if (cost >= 0) { solved++; totalCost += cost; }
            }
        }
        catch
        {
            Console.WriteLine("0 0");
            return;
        }

        Console.WriteLine($"{solved} {totalCost}");
    }

    static void ParseCoords(string s, ref long x, ref long y)
    {
        var m = Regex.Matches(s, @"-?\d+");
        if (m.Count >= 2)
        {
            x = long.Parse(m[0].Value);
            y = long.Parse(m[1].Value);
        }
    }

    static long Solve(long ax, long ay, long bx, long by, long px, long py)
    {
        long D = ax * by - ay * bx;
        if (D == 0) return -1;
        long numA = px * by - py * bx;
        long numB = py * ax - px * ay;
        if (numA % D != 0 || numB % D != 0) return -1;
        long a = numA / D;
        long b = numB / D;
        if (a < 0 || b < 0) return -1;
        return 3 * a + b;
    }
}