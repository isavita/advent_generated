
using System;
using System.Collections.Generic;
using System.IO;

struct Coord
{
    public long X;
    public long Y;
}

class Program
{
    static long HexToLong(string hex)
    {
        return Convert.ToInt64(hex, 16);
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var vertices = new List<Coord> { new Coord { X = 0, Y = 0 } };
        var current = vertices[0];

        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            var parts = line.Split(' ');
            if (parts.Length < 3) continue;

            var color = parts[2];
            var dirChar = color[7];
            var lengthHex = color.Substring(2, 5);
            var len = HexToLong(lengthHex);

            long dx = 0, dy = 0;
            switch (dirChar)
            {
                case '3': dx = 0;  dy = -1; break; // Up
                case '2': dx = -1; dy = 0;  break; // Left
                case '1': dx = 0;  dy = 1;  break; // Down
                case '0': dx = 1;  dy = 0;  break; // Right
                default: throw new InvalidOperationException("Invalid direction");
            }

            current = new Coord
            {
                X = current.X + dx * len,
                Y = current.Y + dy * len
            };
            vertices.Add(current);
        }

        int n = vertices.Count;
        long shoelace = 0;
        long perimeter = 0;

        for (int i = 0; i < n; i++)
        {
            int j = (i + 1) % n;
            var vi = vertices[i];
            var vj = vertices[j];

            shoelace += vi.X * vj.Y - vi.Y * vj.X;
            perimeter += Math.Abs(vi.X - vj.X) + Math.Abs(vi.Y - vj.Y);
        }

        long area = Math.Abs(shoelace) / 2;
        long result = area + (perimeter / 2) + 1;
        Console.WriteLine(result);
    }
}
