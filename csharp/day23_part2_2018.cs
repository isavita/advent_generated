using System;
using System.Collections.Generic;
using System.IO;

struct Coord
{
    public long X, Y, Z;
    public Coord(long x, long y, long z) { X = x; Y = y; Z = z; }
}
struct Bot
{
    public Coord Pos;
    public long R;
    public Bot(Coord p, long r) { Pos = p; R = r; }
}
class Program
{
    static long Dist(Coord a, Coord b) => Math.Abs(a.X - b.X) + Math.Abs(a.Y - b.Y) + Math.Abs(a.Z - b.Z);
    static int InRange(List<Bot> bots, Coord p)
    {
        int c = 0;
        foreach (var b in bots)
            if (Dist(b.Pos, p) <= b.R) c++;
        return c;
    }
    static long Solve(List<Bot> bots)
    {
        var zero = new Coord(0, 0, 0);
        long zoom = 1L << 30;
        var tl = new Coord(0, 0, 0);
        var br = new Coord(0, 0, 0);
        while (true)
        {
            var sb = new List<Bot>(bots.Count);
            foreach (var b in bots)
                sb.Add(new Bot(
                    new Coord(b.Pos.X / zoom, b.Pos.Y / zoom, b.Pos.Z / zoom),
                    b.R / zoom));
            Coord best = new Coord(0, 0, 0);
            int bestCnt = -1;
            for (long x = tl.X; x <= br.X; x++)
                for (long y = tl.Y; y <= br.Y; y++)
                    for (long z = tl.Z; z <= br.Z; z++)
                    {
                        var cur = new Coord(x, y, z);
                        int cnt = InRange(sb, cur);
                        if (cnt < bestCnt) continue;
                        if (cnt == bestCnt && Dist(zero, cur) >= Dist(zero, best)) continue;
                        best = cur;
                        bestCnt = cnt;
                    }
            tl = new Coord((best.X - 1) << 1, (best.Y - 1) << 1, (best.Z - 1) << 1);
            br = new Coord((best.X + 1) << 1, (best.Y + 1) << 1, (best.Z + 1) << 1);
            zoom >>= 1;
            if (zoom == 0) return Dist(zero, best);
        }
    }
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var bots = new List<Bot>(lines.Length);
        foreach (var line in lines)
        {
            var p = line.Substring(line.IndexOf('<') + 1);
            var parts = p.Split(new[] { ',', '>', 'r', '=', ' ' }, StringSplitOptions.RemoveEmptyEntries);
            long x = long.Parse(parts[0]);
            long y = long.Parse(parts[1]);
            long z = long.Parse(parts[2]);
            long r = long.Parse(parts[3]);
            bots.Add(new Bot(new Coord(x, y, z), r));
        }
        Console.WriteLine(Solve(bots));
    }
}
