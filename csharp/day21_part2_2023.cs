
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static int max_size = 0;
    static int startX = -1, startY = -1;
    static char[,] grid;
    static readonly int[] dx = { 0, 0, 1, -1 };
    static readonly int[] dy = { 1, -1, 0, 0 };

    struct Point
    {
        public long x, y;
        public Point(long x, long y) { this.x = x; this.y = y; }
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        max_size = lines.Length;
        grid = new char[max_size, max_size];
        for (int y = 0; y < max_size; y++)
        {
            var line = lines[y];
            for (int x = 0; x < max_size; x++)
            {
                char c = line[x];
                if (c == 'S') { startX = x; startY = y; c = '.'; }
                grid[y, x] = c;
            }
        }

        long[] done = new long[3];
        int doneCount = 0;
        var seen = new HashSet<Point>();
        var current = new HashSet<Point> { new Point(startX, startY) };

        for (int i = 0; ; i++)
        {
            if (doneCount < 3 && (i % max_size) == (max_size - 1) / 2)
            {
                done[doneCount++] = current.Count;
                if (doneCount == 3) break;
            }

            var next = new HashSet<Point>();
            foreach (var p in current)
            {
                for (int k = 0; k < 4; k++)
                {
                    long nx = p.x + dx[k], ny = p.y + dy[k];
                    long mx = (nx % max_size + max_size) % max_size;
                    long my = (ny % max_size + max_size) % max_size;
                    if (grid[my, mx] != '#') next.Add(new Point(nx, ny));
                }
            }
            current = next;
        }

        long n = 26501365 / max_size;
        long res = quadratic(n, done[0], done[1], done[2]);
        Console.WriteLine(res);
    }

    static long quadratic(long n, long a, long b, long c)
    {
        long term1 = a;
        long term2 = n * (b - a);
        long term3 = (n * (n - 1) / 2) * (c - 2 * b + a);
        return term1 + term2 + term3;
    }
}
