
using System;
using System.IO;
using System.Collections.Generic;
using System.Text.RegularExpressions;

class Program
{
    const int MAX_DIM = 50;
    const int WALL_THRESHOLD = 400;

    struct Point { public int X, Y; public Point(int x, int y) { X = x; Y = y; } }
    struct Node { public int Used, Avail; }

    static Node[,] nodes = new Node[MAX_DIM, MAX_DIM];
    static int width, height;

    static int BFS(Point goal, Point from, Point to)
    {
        if (from.X == to.X && from.Y == to.Y) return 0;
        var depth = new int[MAX_DIM, MAX_DIM];
        for (int i = 0; i < width; i++)
            for (int j = 0; j < height; j++)
                depth[i, j] = -1;
        var q = new Queue<Point>();
        depth[from.X, from.Y] = 0;
        q.Enqueue(from);
        var dirs = new[] { (0, 1), (0, -1), (1, 0), (-1, 0) };
        while (q.Count > 0)
        {
            var p = q.Dequeue();
            if (p.X == to.X && p.Y == to.Y) return depth[p.X, p.Y];
            foreach (var d in dirs)
            {
                int nx = p.X + d.Item1, ny = p.Y + d.Item2;
                if (nx < 0 || ny < 0 || nx >= width || ny >= height) continue;
                if (nx == goal.X && ny == goal.Y) continue;
                if (nodes[nx, ny].Used > WALL_THRESHOLD) continue;
                if (depth[nx, ny] != -1) continue;
                depth[nx, ny] = depth[p.X, p.Y] + 1;
                q.Enqueue(new Point(nx, ny));
            }
        }
        return -1;
    }

    static int MinMoves()
    {
        var goal = new Point(width - 1, 0);
        Point hole = new Point(-1, -1);
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                if (nodes[x, y].Used == 0) { hole = new Point(x, y); y = height; break; }
        if (hole.X == -1) return -1;
        int moves = 0;
        while (goal.X != 0 || goal.Y != 0)
        {
            var target = new Point(goal.X - 1, goal.Y);
            int d = BFS(goal, hole, target);
            if (d == -1) return -1;
            moves += d + 1;
            var tmp = goal;
            goal = target;
            hole = tmp;
        }
        return moves;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        if (lines.Length < 3) return;
        var regex = new Regex(@"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%");
        int maxX = 0, maxY = 0;
        for (int i = 2; i < lines.Length; i++)
        {
            var m = regex.Match(lines[i]);
            if (!m.Success) continue;
            int x = int.Parse(m.Groups[1].Value);
            int y = int.Parse(m.Groups[2].Value);
            int used = int.Parse(m.Groups[4].Value);
            int avail = int.Parse(m.Groups[5].Value);
            nodes[x, y] = new Node { Used = used, Avail = avail };
            if (x > maxX) maxX = x;
            if (y > maxY) maxY = y;
        }
        width = maxX + 1;
        height = maxY + 1;
        int result = MinMoves();
        if (result != -1) Console.WriteLine(result);
    }
}
