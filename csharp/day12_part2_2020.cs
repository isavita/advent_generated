
using System;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");

        // Part One
        int x = 0, y = 0, dir = 0;
        foreach (var line in lines)
        {
            int val = int.Parse(line.Substring(1));
            switch (line[0])
            {
                case 'N': y += val; break;
                case 'S': y -= val; break;
                case 'E': x += val; break;
                case 'W': x -= val; break;
                case 'L': dir = (dir + val) % 360; break;
                case 'R': dir = (dir - val + 360) % 360; break;
                case 'F':
                    switch (dir)
                    {
                        case 0: x += val; break;
                        case 90: y -= val; break;
                        case 180: x -= val; break;
                        case 270: y += val; break;
                    }
                    break;
            }
        }
        Console.WriteLine(Math.Abs(x) + Math.Abs(y));

        // Part Two
        int sx = 0, sy = 0, wx = 10, wy = 1;
        foreach (var line in lines)
        {
            int val = int.Parse(line.Substring(1));
            switch (line[0])
            {
                case 'N': wy += val; break;
                case 'S': wy -= val; break;
                case 'E': wx += val; break;
                case 'W': wx -= val; break;
                case 'L':
                    for (int i = 0; i < val / 90; i++) { int t = wx; wx = -wy; wy = t; }
                    break;
                case 'R':
                    for (int i = 0; i < val / 90; i++) { int t = wx; wx = wy; wy = -t; }
                    break;
                case 'F':
                    sx += wx * val;
                    sy += wy * val;
                    break;
            }
        }
        Console.WriteLine(Math.Abs(sx) + Math.Abs(sy));
    }
}
