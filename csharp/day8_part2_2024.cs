using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    const int MAX_H = 500;
    const int MAX_W = 500;

    struct Coord { public int y; public int x; public Coord(int y, int x) { this.y = y; this.x = x; } }

    static long Gcd(long a, long b)
    {
        a = Math.Abs(a);
        b = Math.Abs(b);
        while (b != 0)
        {
            long t = a % b;
            a = b;
            b = t;
        }
        return a;
    }

    static void Main()
    {
        string path = "input.txt";
        if (!File.Exists(path))
        {
            Console.WriteLine("0");
            return;
        }

        string[] lines = File.ReadAllLines(path);
        char[,] grid = new char[MAX_H, MAX_W];
        int h = 0;
        int w = 0;

        foreach (var raw in lines)
        {
            string line = raw.TrimEnd('\r', '\n');
            if (line.Length == 0) continue;
            if (h == 0)
            {
                w = line.Length;
                if (w > MAX_W) w = MAX_W;
            }
            for (int x = 0; x < w; x++)
            {
                grid[h, x] = (x < line.Length) ? line[x] : '.';
            }
            h++;
            if (h >= MAX_H) break;
        }

        if (h == 0 || w == 0)
        {
            Console.WriteLine("0");
            return;
        }

        List<Coord>[] antennaCoords = new List<Coord>[256];
        for (int i = 0; i < 256; i++) antennaCoords[i] = new List<Coord>();
        for (int y = 0; y < h; y++)
        {
            for (int x = 0; x < w; x++)
            {
                char ch = grid[y, x];
                if (ch != '.')
                {
                    int idx = (byte)ch;
                    antennaCoords[idx].Add(new Coord(y, x));
                }
            }
        }

        bool[,] isAntinode = new bool[MAX_H, MAX_W];
        long antinodeCount = 0;

        for (int f = 0; f < 256; f++)
        {
            var list = antennaCoords[f];
            int n = list.Count;
            if (n < 2) continue;
            for (int i = 0; i < n; i++)
            {
                for (int j = i + 1; j < n; j++)
                {
                    Coord A = list[i];
                    Coord B = list[j];
                    long dy = (long)B.y - A.y;
                    long dx = (long)B.x - A.x;
                    long g = Gcd(dy, dx);
                    if (g == 0) continue;
                    long sy = dy / g;
                    long sx = dx / g;
                    if (sx < 0 || (sx == 0 && sy < 0))
                    {
                        sx = -sx;
                        sy = -sy;
                    }
                    long c = sy * A.x - sx * A.y;

                    if (sy == 0)
                    {
                        if (sx == 0) continue;
                        long absC = Math.Abs(c);
                        if (absC % sx == 0)
                        {
                            long yLine = -c / sx;
                            if (yLine >= 0 && yLine < h)
                            {
                                int iy = (int)yLine;
                                for (int ix = 0; ix < w; ix++)
                                {
                                    if (!isAntinode[iy, ix])
                                    {
                                        isAntinode[iy, ix] = true;
                                        antinodeCount++;
                                    }
                                }
                            }
                        }
                    }
                    else if (sx == 0)
                    {
                        long absC = Math.Abs(c);
                        if (absC % sy == 0)
                        {
                            long xLine = c / sy;
                            if (xLine >= 0 && xLine < w)
                            {
                                int ix = (int)xLine;
                                for (int iy = 0; iy < h; iy++)
                                {
                                    if (!isAntinode[iy, ix])
                                    {
                                        isAntinode[iy, ix] = true;
                                        antinodeCount++;
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        for (int iy = 0; iy < h; iy++)
                        {
                            long num = c + sx * (long)iy;
                            if (num % sy == 0)
                            {
                                long xLine = num / sy;
                                if (xLine >= 0 && xLine < w)
                                {
                                    int ix = (int)xLine;
                                    if (!isAntinode[iy, ix])
                                    {
                                        isAntinode[iy, ix] = true;
                                        antinodeCount++;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        Console.WriteLine(antinodeCount);
    }
}