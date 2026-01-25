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
    static readonly int[] dx = { 1, -1, 0, 0 };
    static readonly int[] dy = { 0, 0, 1, -1 };
    static int h, w;
    static char[,] grid;
    static bool[,] wall;
    static Point S, E;
    static List<Point> tracks = new List<Point>();

    static void ReadInput()
    {
        var lines = File.ReadAllLines("input.txt");
        h = lines.Length;
        w = 0;
        foreach (var l in lines) if (l.Length > w) w = l.Length;
        grid = new char[h, w];
        wall = new bool[h, w];
        for (int i = 0; i < h; i++)
        {
            var line = lines[i];
            for (int j = 0; j < w; j++)
            {
                char ch = j < line.Length ? line[j] : ' ';
                grid[i, j] = ch;
                if (ch == '#') wall[i, j] = true;
                else if (ch == 'S') S = new Point(i, j);
                else if (ch == 'E') E = new Point(i, j);
                if (ch != '#' && ch != ' ') tracks.Add(new Point(i, j));
            }
        }
    }

    static int[,] BFS(Point start)
    {
        var dist = new int[h, w];
        for (int i = 0; i < h; i++) for (int j = 0; j < w; j++) dist[i, j] = -1;
        var q = new Queue<Point>();
        dist[start.X, start.Y] = 0;
        q.Enqueue(start);
        while (q.Count > 0)
        {
            var cur = q.Dequeue();
            int cd = dist[cur.X, cur.Y];
            for (int d = 0; d < 4; d++)
            {
                int nx = cur.X + dx[d], ny = cur.Y + dy[d];
                if (nx < 0 || nx >= h || ny < 0 || ny >= w) continue;
                if (wall[nx, ny] || dist[nx, ny] != -1) continue;
                dist[nx, ny] = cd + 1;
                q.Enqueue(new Point(nx, ny));
            }
        }
        return dist;
    }

    static bool IsTrack(int x, int y) => x >= 0 && x < h && y >= 0 && y < w && !wall[x, y];

    static void Main()
    {
        ReadInput();
        var ds = BFS(S);
        var de = BFS(E);
        if (ds[E.X, E.Y] == -1)
        {
            Console.WriteLine(0);
            return;
        }
        int normal = ds[E.X, E.Y];
        int cheats = 0;
        foreach (var p in tracks)
        {
            int sd = ds[p.X, p.Y];
            if (sd == -1) continue;
            for (int i = 0; i < 4; i++)
            {
                int mx = p.X + dx[i], my = p.Y + dy[i];
                if (mx < 0 || mx >= h || my < 0 || my >= w) continue;
                for (int j = 0; j < 4; j++)
                {
                    int nx = mx + dx[j], ny = my + dy[j];
                    if (!IsTrack(nx, ny)) continue;
                    int ed = de[nx, ny];
                    if (ed == -1) continue;
                    int newCost = sd + 2 + ed;
                    if (normal - newCost >= 100) cheats++;
                }
            }
        }
        Console.WriteLine(cheats);
    }
}
