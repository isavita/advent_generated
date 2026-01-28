
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

struct Coord : IComparable<Coord>
{
    public int X, Y;
    public Coord(int x, int y) { X = x; Y = y; }
    public int CompareTo(Coord other) => Y != other.Y ? Y.CompareTo(other.Y) : X.CompareTo(other.X);
    public override bool Equals(object obj) => obj is Coord o && X == o.X && Y == o.Y;
    public override int GetHashCode() => HashCode.Combine(X, Y);
    public static Coord operator +(Coord a, Coord b) => new Coord(a.X + b.X, a.Y + b.Y);
}

struct Edge
{
    public Coord Start, End;
    public int Weight;
    public Edge(Coord s, Coord e, int w) { Start = s; End = e; Weight = w; }
    public override bool Equals(object obj) =>
        obj is Edge o && Start.Equals(o.Start) && End.Equals(o.End) && Weight == o.Weight;
    public override int GetHashCode() => HashCode.Combine(Start, End, Weight);
}

class Program
{
    static readonly Coord North = new Coord(0, -1);
    static readonly Coord South = new Coord(0, 1);
    static readonly Coord West  = new Coord(-1, 0);
    static readonly Coord East  = new Coord(1, 0);
    static readonly Coord[] Dir4 = { North, South, West, East };
    const char Wall = '#';
    const char Empty = '.';

    static bool InBounds(Coord c, int w, int h) => c.X >= 0 && c.X < w && c.Y >= 0 && c.Y < h;
    static bool ValidNeighbor(Coord c, Dictionary<Coord, char> data, int w, int h) =>
        InBounds(c, w, h) && (!data.TryGetValue(c, out var ch) || ch != Wall);

    static List<Coord> Neighbors(Coord c, Dictionary<Coord, char> data, int w, int h)
    {
        var list = new List<Coord>();
        foreach (var d in Dir4)
        {
            var n = c + d;
            if (ValidNeighbor(n, data, w, h)) list.Add(n);
        }
        return list;
    }

    static (Dictionary<Coord, char> data, int w, int h) Parse(IReadOnlyList<string> lines)
    {
        int h = lines.Count, w = h == 0 ? 0 : lines[0].Length;
        var dict = new Dictionary<Coord, char>();
        for (int y = 0; y < h; ++y)
            for (int x = 0; x < w; ++x)
                if (lines[y][x] != Empty)
                    dict[new Coord(x, y)] = lines[y][x];
        return (dict, w, h);
    }

    static HashSet<Edge> BfsEdges(Coord start, Dictionary<Coord, char> data, int w, int h, HashSet<Coord> verts)
    {
        var q = new Queue<Coord>();
        var reached = new HashSet<Coord>();
        var dist = new Dictionary<Coord, int>();
        q.Enqueue(start);
        reached.Add(start);
        dist[start] = 0;
        var edges = new HashSet<Edge>();
        while (q.Count > 0)
        {
            var cur = q.Dequeue();
            if (verts.Contains(cur) && !cur.Equals(start))
            {
                edges.Add(new Edge(start, cur, dist[cur]));
                continue;
            }
            foreach (var n in Neighbors(cur, data, w, h))
                if (reached.Add(n))
                {
                    dist[n] = dist[cur] + 1;
                    q.Enqueue(n);
                }
        }
        return edges;
    }

    static (HashSet<Coord> vertices, Dictionary<Coord, List<Edge>> edges) BuildGraph(
        Dictionary<Coord, char> data, int w, int h, Coord start, Coord end)
    {
        var verts = new HashSet<Coord> { start, end };
        for (int y = 0; y < h; ++y)
            for (int x = 0; x < w; ++x)
            {
                var c = new Coord(x, y);
                if (!data.TryGetValue(c, out var ch) || ch != Wall)
                    if (Neighbors(c, data, w, h).Count > 2)
                        verts.Add(c);
            }
        var edges = new Dictionary<Coord, List<Edge>>();
        foreach (var v in verts)
            edges[v] = BfsEdges(v, data, w, h, verts).ToList();
        return (verts, edges);
    }

    static int Dfs(Coord cur, Coord end, Dictionary<Coord, List<Edge>> edges,
        HashSet<Coord> seen)
    {
        if (cur.Equals(end)) return 0;
        seen.Add(cur);
        int best = -1;
        if (edges.TryGetValue(cur, out var list))
            foreach (var e in list)
                if (!seen.Contains(e.End))
                {
                    int d = Dfs(e.End, end, edges, seen);
                    if (d >= 0) best = Math.Max(best, d + e.Weight);
                }
        seen.Remove(cur);
        return best;
    }

    static int Solve(IReadOnlyList<string> lines)
    {
        var (data, w, h) = Parse(lines);
        var start = new Coord(1, 0);
        var end   = new Coord(w - 2, h - 1);
        var (_, graphEdges) = BuildGraph(data, w, h, start, end);
        var seen = new HashSet<Coord>();
        return Dfs(start, end, graphEdges, seen);
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        Console.WriteLine(Solve(lines));
    }
}
