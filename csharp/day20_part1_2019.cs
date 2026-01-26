
using System;
using System.IO;
using System.Collections.Generic;

struct Pt
{
    public int X, Y;
    public Pt(int x, int y) { X = x; Y = y; }
    public override bool Equals(object o) => o is Pt p && X == p.X && Y == p.Y;
    public override int GetHashCode() => X * 397 ^ Y;
}

class Program
{
    static Dictionary<string, List<Pt>> FindPortals(char[][] m)
    {
        var d = new Dictionary<string, List<Pt>>();
        int R = m.Length, C = m[0].Length;
        for (int r = 0; r < R; r++)
        for (int c = 0; c < C; c++)
        {
            if (!char.IsUpper(m[r][c])) continue;
            string name;
            Pt p;
            if (r + 1 < R && char.IsUpper(m[r + 1][c]))
            {
                name = $"{m[r][c]}{m[r + 1][c]}";
                p = (r + 2 < R && m[r + 2][c] == '.') ? new Pt(c, r + 2) : new Pt(c, r - 1);
            }
            else if (c + 1 < C && char.IsUpper(m[r][c + 1]))
            {
                name = $"{m[r][c]}{m[r][c + 1]}";
                p = (c + 2 < C && m[r][c + 2] == '.') ? new Pt(c + 2, r) : new Pt(c - 1, r);
            }
            else continue;
            if (!d.ContainsKey(name)) d[name] = new List<Pt>();
            d[name].Add(p);
        }
        return d;
    }

    static int Bfs(char[][] m, Pt start, Pt end, Dictionary<string, List<Pt>> portals)
    {
        int R = m.Length, C = m[0].Length;
        var q = new Queue<(Pt p, int s)>();
        var vis = new HashSet<Pt>();
        q.Enqueue((start, 0));
        vis.Add(start);
        int[] dr = {-1,1,0,0}, dc = {0,0,-1,1};

        while (q.Count > 0)
        {
            var (cur, steps) = q.Dequeue();
            if (cur.Equals(end)) return steps;
            foreach (var i in new[] {0,1,2,3})
            {
                int nr = cur.Y + dr[i], nc = cur.X + dc[i];
                if (nr>=0 && nr<R && nc>=0 && nc<C && m[nr][nc]=='.')
                {
                    var nxt = new Pt(nc, nr);
                    if (vis.Add(nxt)) q.Enqueue((nxt, steps+1));
                }
            }
            foreach (var kv in portals)
            {
                var name = kv.Key;
                if (name=="AA"||name=="ZZ") continue;
                var pts = kv.Value;
                if (!pts.Contains(cur)) continue;
                var other = pts[0].Equals(cur) ? pts[1] : pts[0];
                if (vis.Add(other)) q.Enqueue((other, steps+1));
            }
        }
        return -1;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var maze = new char[lines.Length][];
        for (int i=0;i<lines.Length;i++) maze[i]=lines[i].ToCharArray();

        var portals = FindPortals(maze);
        var start = portals["AA"][0];
        var end   = portals["ZZ"][0];
        Console.WriteLine(Bfs(maze, start, end, portals));
    }
}
