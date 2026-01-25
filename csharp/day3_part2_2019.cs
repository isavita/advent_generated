using System;
using System.IO;
using System.Collections.Generic;

struct Point : IEquatable<Point>
{
    public int X, Y;
    public Point(int x, int y) { X = x; Y = y; }
    public bool Equals(Point other) => X == other.X && Y == other.Y;
    public override bool Equals(object obj) => obj is Point p && Equals(p);
    public override int GetHashCode() => HashCode.Combine(X, Y);
}

class Program
{
    static Dictionary<Point, int> GetSteps(string path)
    {
        var dict = new Dictionary<Point, int>();
        var parts = path.Split(',', StringSplitOptions.RemoveEmptyEntries);
        int x = 0, y = 0, steps = 0;
        foreach (var part in parts)
        {
            char dir = part[0];
            int dist = int.Parse(part.Substring(1));
            for (int i = 0; i < dist; i++)
            {
                steps++;
                switch (dir)
                {
                    case 'U': y++; break;
                    case 'D': y--; break;
                    case 'L': x--; break;
                    case 'R': x++; break;
                }
                var p = new Point(x, y);
                if (!dict.ContainsKey(p))
                    dict[p] = steps;
            }
        }
        return dict;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        if (lines.Length < 2) return;
        var wire1 = GetSteps(lines[0].Trim());
        var wire2 = GetSteps(lines[1].Trim());
        int min = int.MaxValue;
        foreach (var kvp in wire1)
        {
            if (wire2.TryGetValue(kvp.Key, out int steps2))
            {
                int total = kvp.Value + steps2;
                if (total < min) min = total;
            }
        }
        Console.WriteLine(min);
    }
}