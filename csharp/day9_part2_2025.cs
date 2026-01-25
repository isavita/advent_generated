using System;
using System.Collections.Generic;
using System.IO;

struct Point
{
    public int X, Y;
    public Point(int x, int y) { X = x; Y = y; }
}

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var pts = new List<Point>();
        var xsList = new List<int>();
        var ysList = new List<int>();
        foreach (var line in lines)
        {
            var s = line.Trim();
            if (s.Length == 0) continue;
            var parts = s.Split(',');
            if (parts.Length != 2) continue;
            if (!int.TryParse(parts[0].Trim(), out int x)) continue;
            if (!int.TryParse(parts[1].Trim(), out int y)) continue;
            pts.Add(new Point(x, y));
            xsList.Add(x);
            ysList.Add(y);
        }
        if (pts.Count == 0)
        {
            Console.WriteLine("Largest valid area: 0");
            return;
        }

        xsList.Sort();
        ysList.Sort();
        var xs = new List<int>();
        var ys = new List<int>();
        int prev = int.MinValue;
        foreach (var v in xsList) if (v != prev) { xs.Add(v); prev = v; }
        prev = int.MinValue;
        foreach (var v in ysList) if (v != prev) { ys.Add(v); prev = v; }

        var xidx = new Dictionary<int, int>();
        for (int i = 0; i < xs.Count; i++) xidx[xs[i]] = i;
        var yidx = new Dictionary<int, int>();
        for (int i = 0; i < ys.Count; i++) yidx[ys[i]] = i;

        int W = 2 * xs.Count + 1;
        int H = 2 * ys.Count + 1;
        var colW = new long[W];
        var rowH = new long[H];
        colW[0] = 1;
        for (int i = 0; i < xs.Count; i++)
        {
            colW[2 * i + 1] = 1;
            colW[2 * i + 2] = i + 1 < xs.Count ? Math.Max(0, xs[i + 1] - xs[i] - 1) : 1;
        }
        rowH[0] = 1;
        for (int i = 0; i < ys.Count; i++)
        {
            rowH[2 * i + 1] = 1;
            rowH[2 * i + 2] = i + 1 < ys.Count ? Math.Max(0, ys[i + 1] - ys[i] - 1) : 1;
        }

        var grid = new byte[H, W];
        for (int i = 0; i < pts.Count; i++)
        {
            var a = pts[i];
            var b = pts[(i + 1) % pts.Count];
            int gx1 = 2 * xidx[a.X] + 1, gy1 = 2 * yidx[a.Y] + 1;
            int gx2 = 2 * xidx[b.X] + 1, gy2 = 2 * yidx[b.Y] + 1;
            if (gx1 == gx2)
            {
                int y0 = Math.Min(gy1, gy2), y1 = Math.Max(gy1, gy2);
                for (int y = y0; y <= y1; y++) if (rowH[y] > 0) grid[y, gx1] = 1;
            }
            else
            {
                int x0 = Math.Min(gx1, gx2), x1 = Math.Max(gx1, gx2);
                for (int x = x0; x <= x1; x++) if (colW[x] > 0) grid[gy1, x] = 1;
            }
        }

        var q = new Point[W * H];
        int qh = 0, qt = 0;
        q[qt++] = new Point(0, 0);
        grid[0, 0] = 2;
        int[] dx = { 0, 0, 1, -1 };
        int[] dy = { 1, -1, 0, 0 };
        while (qh < qt)
        {
            var cur = q[qh++];
            for (int d = 0; d < 4; d++)
            {
                int nx = cur.X + dx[d];
                int ny = cur.Y + dy[d];
                if (nx >= 0 && nx < W && ny >= 0 && ny < H && grid[ny, nx] == 0)
                {
                    grid[ny, nx] = 2;
                    q[qt++] = new Point(nx, ny);
                }
            }
        }

        var P = new long[H, W];
        for (int y = 0; y < H; y++)
        {
            for (int x = 0; x < W; x++)
            {
                long val = grid[y, x] != 2 ? colW[x] * rowH[y] : 0;
                long left = x > 0 ? P[y, x - 1] : 0;
                long up = y > 0 ? P[y - 1, x] : 0;
                long diag = (x > 0 && y > 0) ? P[y - 1, x - 1] : 0;
                P[y, x] = val + left + up - diag;
            }
        }

        long maxArea = 0;
        for (int i = 0; i < pts.Count; i++)
        {
            for (int j = i; j < pts.Count; j++)
            {
                var a = pts[i];
                var b = pts[j];
                long w = Math.Abs((long)a.X - b.X) + 1;
                long h = Math.Abs((long)a.Y - b.Y) + 1;
                long area = w * h;
                if (area <= maxArea) continue;
                int gx1 = 2 * xidx[a.X] + 1, gy1 = 2 * yidx[a.Y] + 1;
                int gx2 = 2 * xidx[b.X] + 1, gy2 = 2 * yidx[b.Y] + 1;
                if (gx1 > gx2) { int t = gx1; gx1 = gx2; gx2 = t; }
                if (gy1 > gy2) { int t = gy1; gy1 = gy2; gy2 = t; }
                long total = P[gy2, gx2];
                long leftSum = gx1 > 0 ? P[gy2, gx1 - 1] : 0;
                long upSum = gy1 > 0 ? P[gy1 - 1, gx2] : 0;
                long diagSum = (gx1 > 0 && gy1 > 0) ? P[gy1 - 1, gx1 - 1] : 0;
                long valid = total - leftSum - upSum + diagSum;
                if (valid == area) maxArea = area;
            }
        }

        Console.WriteLine($"Largest valid area: {maxArea}");
    }
}
