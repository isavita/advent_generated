
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // read all lines
        string[] lines = File.ReadAllLines("input.txt");
        int height = lines.Length;
        if (height == 0) return;
        int width = lines[0].Length;

        // store grid as char[][] for fast indexing
        char[][] grid = new char[height][];
        for (int y = 0; y < height; y++)
            grid[y] = lines[y].ToCharArray();

        // locate start position 'S'
        int sx = 0, sy = 0;
        bool found = false;
        for (int y = 0; y < height && !found; y++)
            for (int x = 0; x < width; x++)
                if (grid[y][x] == 'S')
                {
                    sx = x; sy = y; found = true; break;
                }

        int splits = 0;
        bool[] active = new bool[width];
        bool[] next = new bool[width];
        active[sx] = true;                     // start column is active

        // simulate line‑by‑line propagation
        for (int y = sy; y < height; y++)
        {
            Array.Clear(next, 0, width);
            for (int x = 0; x < width; x++)
            {
                if (!active[x]) continue;

                if (grid[y][x] == '^')
                {
                    splits++;
                    if (x > 0)       next[x - 1] = true;
                    if (x + 1 < width) next[x + 1] = true;
                }
                else
                {
                    next[x] = true;
                }
            }

            // swap active / next buffers
            var tmp = active;
            active = next;
            next = tmp;

            // stop if no column remains active
            bool any = false;
            for (int i = 0; i < width; i++)
                if (active[i]) { any = true; break; }
            if (!any) break;
        }

        Console.WriteLine(splits);
    }
}
