using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

struct Point { public int X, Y, Z; }
struct Edge { public int U, V; public long D; }

class Program
{
    static int[] parent, sz;
    static int Find(int x)
    {
        while (parent[x] != x) { parent[x] = parent[parent[x]]; x = parent[x]; }
        return x;
    }
    static void Union(int a, int b)
    {
        int ra = Find(a), rb = Find(b);
        if (ra == rb) return;
        if (sz[ra] < sz[rb]) { var t = ra; ra = rb; rb = t; }
        parent[rb] = ra;
        sz[ra] += sz[rb];
    }
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var pts = new List<Point>();
        foreach (var l in lines)
        {
            var s = l.Replace(",", " ").Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            if (s.Length != 3) continue;
            pts.Add(new Point { X = int.Parse(s[0]), Y = int.Parse(s[1]), Z = int.Parse(s[2]) });
        }
        int n = pts.Count;
        if (n < 2) return;
        long edgeCount = (long)n * (n - 1) / 2;
        var edges = new Edge[edgeCount];
        int idx = 0;
        for (int i = 0; i < n; ++i)
            for (int j = i + 1; j < n; ++j)
            {
                long dx = (long)pts[i].X - pts[j].X;
                long dy = (long)pts[i].Y - pts[j].Y;
                long dz = (long)pts[i].Z - pts[j].Z;
                edges[idx++] = new Edge { U = i, V = j, D = dx * dx + dy * dy + dz * dz };
            }
        Array.Sort(edges, (a, b) => a.D.CompareTo(b.D));
        parent = new int[n];
        sz = new int[n];
        for (int i = 0; i < n; ++i) { parent[i] = i; sz[i] = 1; }
        int limit = edges.Length < 1000 ? edges.Length : 1000;
        for (int i = 0; i < limit; ++i) Union(edges[i].U, edges[i].V);
        var top = new int[3];
        for (int i = 0; i < n; ++i)
            if (parent[i] == i)
            {
                int s = sz[i];
                if (s > top[0]) { top[2] = top[1]; top[1] = top[0]; top[0] = s; }
                else if (s > top[1]) { top[2] = top[1]; top[1] = s; }
                else if (s > top[2]) top[2] = s;
            }
        ulong result = 1;
        for (int i = 0; i < 3 && top[i] > 0; ++i) result *= (ulong)top[i];
        Console.WriteLine(result);
    }
}