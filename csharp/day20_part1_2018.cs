using System;
using System.Collections.Generic;
using System.IO;

struct Point : IEquatable<Point>
{
    public int X;
    public int Y;
    public Point(int x, int y) { X = x; Y = y; }
    public bool Equals(Point other) => X == other.X && Y == other.Y;
    public override bool Equals(object obj) => obj is Point p && Equals(p);
    public override int GetHashCode() => unchecked((X * 397) ^ Y);
}

class Program
{
    static void Main(string[] args)
    {
        string data = File.ReadAllText("input.txt").Trim();
        if (data.Length < 2)
        {
            Console.WriteLine(0);
            return;
        }
        string regex = data.Substring(1, data.Length - 2);
        var dm = BuildMap(regex);
        int ans = FindFurthestRoom(dm);
        Console.WriteLine(ans);
    }

    static Point Move(Point p, char dir)
    {
        switch (dir)
        {
            case 'N': return new Point(p.X, p.Y - 1);
            case 'S': return new Point(p.X, p.Y + 1);
            case 'E': return new Point(p.X + 1, p.Y);
            case 'W': return new Point(p.X - 1, p.Y);
            default: return p;
        }
    }

    static Dictionary<Point, HashSet<Point>> BuildMap(string regex)
    {
        var dm = new Dictionary<Point, HashSet<Point>>();
        var stack = new Stack<Point>();
        var cp = new Point(0, 0);

        foreach (char c in regex)
        {
            if (c == '(')
            {
                stack.Push(cp);
            }
            else if (c == '|')
            {
                cp = stack.Peek();
            }
            else if (c == ')')
            {
                cp = stack.Pop();
            }
            else
            {
                Point np = Move(cp, c);
                if (!dm.ContainsKey(cp)) dm[cp] = new HashSet<Point>();
                dm[cp].Add(np);
                cp = np;
            }
        }

        return dm;
    }

    static int FindFurthestRoom(Dictionary<Point, HashSet<Point>> dm)
    {
        var visited = new Dictionary<Point, int>();
        var queue = new Queue<Point>();
        queue.Enqueue(new Point(0, 0));
        int maxDoors = 0;

        while (queue.Count > 0)
        {
            var p = queue.Dequeue();
            if (!dm.TryGetValue(p, out var neighbors)) continue;

            foreach (var np in neighbors)
            {
                if (!visited.ContainsKey(np))
                {
                    int distToP = 0;
                    visited.TryGetValue(p, out distToP);
                    int dist = distToP + 1;
                    visited[np] = dist;
                    if (dist > maxDoors) maxDoors = dist;
                    queue.Enqueue(np);
                }
            }
        }

        return maxDoors;
    }
}