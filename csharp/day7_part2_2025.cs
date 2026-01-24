
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        if (lines.Length == 0) { Console.WriteLine(0); return; }

        int h = lines.Length, w = lines[0].Length;
        int sx = -1, sy = -1;
        for (int y = 0; y < h && sx < 0; y++)
            for (int x = 0; x < w; x++)
                if (lines[y][x] == 'S') { sx = x; sy = y; break; }

        if (sx < 0) return;

        var cur = new Dictionary<int, ulong> { [sx] = 1 };

        for (int y = sy; y < h; y++)
        {
            var nxt = new Dictionary<int, ulong>();
            foreach (var kv in cur)
            {
                int x = kv.Key; ulong cnt = kv.Value;
                if (x >= 0 && x < w && lines[y][x] == '^')
                {
                    Increment(nxt, x - 1, cnt);
                    Increment(nxt, x + 1, cnt);
                }
                else Increment(nxt, x, cnt);
            }
            cur = nxt;
        }

        ulong tot = 0;
        foreach (var v in cur.Values) tot += v;
        Console.WriteLine(tot);
    }

    static void Increment(Dictionary<int, ulong> d, int k, ulong v)
    {
        d.TryGetValue(k, out var old);
        d[k] = old + v;
    }
}
