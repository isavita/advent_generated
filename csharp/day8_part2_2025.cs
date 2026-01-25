
using System;
using System.Collections.Generic;
using System.IO;

struct Point
{
    public int X, Y, Z;
    public Point(int x, int y, int z) => (X, Y, Z) = (x, y, z);
}

struct Edge
{
    public int U, V;
    public long D;
    public Edge(int u, int v, long d) => (U, V, D) = (u, v, d);
}

static class Program
{
    static long DistSq(Point a, Point b)
    {
        long dx = a.X - b.X, dy = a.Y - b.Y, dz = a.Z - b.Z;
        return dx * dx + dy * dy + dz * dz;
    }

    static int Find(int[] p, int x)
    {
        while (p[x] != x) { p[x] = p[p[x]]; x = p[x]; }
        return x;
    }

    static void Union(int[] p, int[] r, int a, int b)
    {
        a = Find(p, a);
        b = Find(p, b);
        if (a == b) return;
        if (r[a] < r[b]) p[a] = b;
        else if (r[a] > r[b]) p[b] = a;
        else { p[b] = a; r[a]++; }
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var points = new List<Point>();
        foreach (var raw in lines)
        {
            var line = raw.Trim();
            if (line.Length == 0) continue;
            var parts = line.Split(',');
            if (parts.Length != 3) continue;
            if (int.TryParse(parts[0], out var x) &&
                int.TryParse(parts[1], out var y) &&
                int.TryParse(parts[2], out var z))
                points.Add(new Point(x, y, z));
        }

        int n = points.Count;
        if (n < 2) return;

        var edges = new Edge[n * (n - 1) / 2];
        int e = 0;
        for (int i = 0; i < n; i++)
            for (int j = i + 1; j < n; j++)
                edges[e++] = new Edge(i, j, DistSq(points[i], points[j]));

        Array.Sort(edges, 0, e, Comparer<Edge>.Create((a, b) => a.D.CompareTo(b.D)));

        var parent = new int[n];
        var rank = new int[n];
        for (int i = 0; i < n; i++) parent[i] = i;

        int comps = n;
        foreach (var edge in edges)
        {
            int ru = Find(parent, edge.U);
            int rv = Find(parent, edge.V);
            if (ru != rv)
            {
                Union(parent, rank, ru, rv);
                if (--comps == 1)
                {
                    var p1 = points[edge.U];
                    var p2 = points[edge.V];
                    Console.WriteLine($"Connected {p1.X},{p1.Y},{p1.Z} and {p2.X},{p2.Y},{p2.Z}");
                    Console.WriteLine($"Product of X coordinates: {(long)p1.X * p2.X}");
                    break;
                }
            }
        }
    }
}
