
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public struct Point
{
    public int X, Y;

    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }

    public override bool Equals(object obj) => obj is Point point && X == point.X && Y == point.Y;
    public override int GetHashCode() => HashCode.Combine(X, Y);
}

public class PriorityQueue<T> where T : IComparable<T>
{
    private List<T> data = new List<T>();

    public void Enqueue(T item)
    {
        data.Add(item);
        int ci = data.Count - 1;
        while (ci > 0)
        {
            int pi = (ci - 1) / 2;
            if (data[ci].CompareTo(data[pi]) >= 0) break;
            T tmp = data[ci]; data[ci] = data[pi]; data[pi] = tmp;
            ci = pi;
        }
    }

    public T Dequeue()
    {
        int li = data.Count - 1;
        T frontItem = data[0];
        data[0] = data[li];
        data.RemoveAt(li);
        --li;
        int pi = 0;
        while (true)
        {
            int ci = pi * 2 + 1;
            if (ci > li) break;
            int rc = ci + 1;
            if (rc <= li && data[rc].CompareTo(data[ci]) < 0) ci = rc;
            if (data[pi].CompareTo(data[ci]) <= 0) break;
            T tmp = data[pi]; data[pi] = data[ci]; data[ci] = tmp;
            pi = ci;
        }
        return frontItem;
    }

    public bool IsEmpty => data.Count == 0;
}

public class Item : IComparable<Item>
{
    public Point Point;
    public int Priority;

    public Item(Point point, int priority)
    {
        Point = point;
        Priority = priority;
    }

    public int CompareTo(Item other) => other.Priority.CompareTo(Priority);
}

class Program
{
    static Dictionary<Point, byte> grid = new Dictionary<Point, byte>();
    static Point start, end;
    static List<Point> asList = new List<Point>();
    static readonly Point[] Neighbors4 = { new Point(0, 1), new Point(0, -1), new Point(1, 0), new Point(-1, 0) };

    static void Main(string[] args)
    {
        string[] lines = File.ReadAllLines("input.txt");

        for (int y = 0; y < lines.Length; y++)
        {
            for (int x = 0; x < lines[y].Length; x++)
            {
                Point p = new Point(x, y);
                byte b = (byte)lines[y][x];
                grid[p] = b;
                if (b == (byte)'S') start = p;
                else if (b == (byte)'E') end = p;
                else if (b == (byte)'a') asList.Add(p);
            }
        }

        grid[start] = (byte)'a';
        grid[end] = (byte)'z';

        var dists = Dijkstra(grid, end);

        int l = dists.TryGetValue(start, out int startDist) ? startDist : int.MaxValue;

        foreach (var a in asList)
        {
            if (dists.TryGetValue(a, out int aDist) && aDist < l) l = aDist;
        }

        Console.WriteLine(l);
    }

    static Dictionary<Point, int> Dijkstra(Dictionary<Point, byte> grid, Point end)
    {
        var pq = new PriorityQueue<Item>();
        var dist = new Dictionary<Point, int>();

        dist[end] = 0;
        pq.Enqueue(new Item(end, 0));

        while (!pq.IsEmpty)
        {
            var curr = pq.Dequeue();
            foreach (var n in Neighbors4)
            {
                Point next = new Point(curr.Point.X + n.X, curr.Point.Y + n.Y);
                if (!grid.ContainsKey(next)) continue;
                if (grid[curr.Point] - grid[next] > 1) continue;
                int nextDist = dist[curr.Point] + 1;
                if (!dist.TryGetValue(next, out int nextDistOld) || nextDist < nextDistOld)
                {
                    dist[next] = nextDist;
                    pq.Enqueue(new Item(next, nextDist));
                }
            }
        }

        return dist;
    }
}
