
using System;
using System.IO;
using System.Collections.Generic;

struct Point
{
    public int X;
    public int Y;
    public Point(int x, int y) { X = x; Y = y; }
    public override int GetHashCode() => X * 31 + Y;
    public override bool Equals(object obj) => obj is Point p && p.X == X && p.Y == Y;
}

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        if (lines.Length < 2) return;

        var wire1 = lines[0].Trim();
        var wire2 = lines[1].Trim();

        var visited = new HashSet<Point>();
        int x = 0, y = 0;
        foreach (var token in wire1.Split(',', StringSplitOptions.RemoveEmptyEntries))
        {
            char dir = token[0];
            int steps = int.Parse(token.AsSpan(1));
            for (int i = 0; i < steps; i++)
            {
                switch (dir)
                {
                    case 'U': y++; break;
                    case 'D': y--; break;
                    case 'L': x--; break;
                    case 'R': x++; break;
                }
                visited.Add(new Point(x, y));
            }
        }

        x = 0; y = 0;
        int minDist = int.MaxValue;
        foreach (var token in wire2.Split(',', StringSplitOptions.RemoveEmptyEntries))
        {
            char dir = token[0];
            int steps = int.Parse(token.AsSpan(1));
            for (int i = 0; i < steps; i++)
            {
                switch (dir)
                {
                    case 'U': y++; break;
                    case 'D': y--; break;
                    case 'L': x--; break;
                    case 'R': x++; break;
                }
                var p = new Point(x, y);
                if (visited.Contains(p))
                {
                    int dist = Math.Abs(x) + Math.Abs(y);
                    if (dist < minDist) minDist = dist;
                }
            }
        }

        Console.WriteLine(minDist);
    }
}
