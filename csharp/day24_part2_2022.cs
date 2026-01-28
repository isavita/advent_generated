using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

struct Blizzard { public int x, y; public char dir; }
struct Point : IEquatable<Point> { public int x, y; public bool Equals(Point o) => x == o.x && y == o.y; public override bool Equals(object o) => o is Point p && Equals(p); public override int GetHashCode() => x * 397 ^ y; }
struct State : IEquatable<State> { public int x, y, t; public bool Equals(State o) => x == o.x && y == o.y && t == o.t; public override bool Equals(object o) => o is State s && Equals(s); public override int GetHashCode() => (x, y, t).GetHashCode(); }

class Program {
    static long Gcd(long a, long b) { while (b != 0) { var r = a % b; a = b; b = r; } return Math.Abs(a); }
    static long Lcm(long a, long b) => a == 0 || b == 0 ? 0 : Math.Abs(a / Gcd(a, b) * b);
    static (HashSet<Point> walls, List<Blizzard> blizzards, int h, int w) ReadInput(string path) {
        var walls = new HashSet<Point>();
        var blizzards = new List<Blizzard>();
        var lines = File.ReadAllLines(path);
        int h = lines.Length, w = lines[0].Length;
        for (int y = 0; y < h; y++) {
            var line = lines[y];
            for (int x = 0; x < w; x++) {
                var c = line[x];
                if (c == '#') walls.Add(new Point { x = x, y = y });
                else if ("<>^v".Contains(c)) blizzards.Add(new Blizzard { x = x, y = y, dir = c });
            }
        }
        return (walls, blizzards, h, w);
    }
    static (Point start, Point end) FindStartEnd(HashSet<Point> walls, int h, int w) {
        Point s = new Point { x = -1, y = 0 }, e = new Point { x = -1, y = h - 1 };
        for (int x = 0; x < w; x++) if (!walls.Contains(new Point { x = x, y = 0 })) { s.x = x; break; }
        for (int x = 0; x < w; x++) if (!walls.Contains(new Point { x = x, y = h - 1 })) { e.x = x; break; }
        return (s, e);
    }
    static bool[,,] Precompute(List<Blizzard> blizzards, int w, int h, long period) {
        var grid = new bool[period, h, w];
        int iw = w - 2, ih = h - 2;
        for (int t = 0; t < period; t++) {
            foreach (var b in blizzards) {
                int nx = b.x, ny = b.y;
                switch (b.dir) {
                    case '>': nx = 1 + ((b.x - 1 + t) % iw); break;
                    case '<': nx = 1 + ((b.x - 1 - t) % iw + iw) % iw; break;
                    case 'v': ny = 1 + ((b.y - 1 + t) % ih); break;
                    case '^': ny = 1 + ((b.y - 1 - t) % ih + ih) % ih; break;
                }
                grid[t, ny, nx] = true;
            }
        }
        return grid;
    }
    static int Bfs(Point start, Point end, HashSet<Point> walls, bool[,,] blizz, long period, int w, int h, int startTime) {
        var q = new Queue<(int x, int y, int t)>();
        var visited = new HashSet<State>();
        q.Enqueue((start.x, start.y, startTime));
        visited.Add(new State { x = start.x, y = start.y, t = startTime % (int)period });
        int[][] dirs = new[] { new[] {0,0}, new[] {1,0}, new[] {-1,0}, new[] {0,1}, new[] {0,-1} };
        while (q.Count > 0) {
            var (x, y, t) = q.Dequeue();
            if (x == end.x && y == end.y) return t;
            int nt = t + 1, ntp = nt % (int)period;
            foreach (var d in dirs) {
                int nx = x + d[0], ny = y + d[1];
                if (nx == end.x && ny == end.y && !blizz[ntp, ny, nx]) return nt;
                if (nx == start.x && ny == start.y && !blizz[ntp, ny, nx]) {
                    var s = new State { x = nx, y = ny, t = ntp };
                    if (visited.Add(s)) q.Enqueue((nx, ny, nt));
                    continue;
                }
                if (nx >= 1 && nx < w - 1 && ny >= 1 && ny < h - 1) {
                    if (walls.Contains(new Point { x = nx, y = ny }) || blizz[ntp, ny, nx]) continue;
                    var s = new State { x = nx, y = ny, t = ntp };
                    if (visited.Add(s)) q.Enqueue((nx, ny, nt));
                }
            }
        }
        return -1;
    }
    static void Main() {
        var (walls, blizzards, h, w) = ReadInput("input.txt");
        var (start, end) = FindStartEnd(walls, h, w);
        long period = Lcm(w - 2, h - 2);
        var blizz = Precompute(blizzards, w, h, period);
        int t1 = Bfs(start, end, walls, blizz, period, w, h, 0);
        int t2 = Bfs(end, start, walls, blizz, period, w, h, t1);
        int t3 = Bfs(start, end, walls, blizz, period, w, h, t2);
        Console.WriteLine(t3);
    }
}