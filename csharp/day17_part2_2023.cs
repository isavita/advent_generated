using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public struct Coord
{
    public int X;
    public int Y;
    public Coord(int x, int y) { X = x; Y = y; }
    public static Coord operator +(Coord a, Coord b) => new Coord(a.X + b.X, a.Y + b.Y);
    public static Coord operator -(Coord a, Coord b) => new Coord(a.X - b.X, a.Y - b.Y);
    public Coord Opposite => new Coord(-X, -Y);
    public static bool operator ==(Coord a, Coord b) => a.X == b.X && a.Y == b.Y;
    public static bool operator !=(Coord a, Coord b) => !(a == b);
    public bool Equals(Coord other) => X == other.X && Y == other.Y;
    public override bool Equals(object obj) => obj is Coord c && Equals(c);
    public override int GetHashCode()
    {
        unchecked { int h = 17; h = h * 31 + X; h = h * 31 + Y; return h; }
    }
}

public struct Info
{
    public Coord Coord;
    public Coord Dir;
    public int NumStraight;
    public Info(Coord coord, Coord dir, int numStraight)
    {
        Coord = coord;
        Dir = dir;
        NumStraight = numStraight;
    }
    public override bool Equals(object obj)
    {
        if (!(obj is Info o)) return false;
        return Coord.Equals(o.Coord) && Dir.Equals(o.Dir) && NumStraight == o.NumStraight;
    }
    public override int GetHashCode()
    {
        unchecked
        {
            int h = 17;
            h = h * 31 + Coord.GetHashCode();
            h = h * 31 + Dir.GetHashCode();
            h = h * 31 + NumStraight.GetHashCode();
            return h;
        }
    }
}

public class Grid
{
    public int Width { get; }
    public int Height { get; }
    public Dictionary<Coord, int> Data { get; }
    public Grid(int width, int height, Dictionary<Coord, int> data)
    {
        Width = width;
        Height = height;
        Data = data;
    }
    public List<Coord> Neighbors4(Coord c)
    {
        var res = new List<Coord>(4);
        var up = new Coord(c.X, c.Y - 1);
        if (up.Y >= 0 && up.Y < Height && up.X >= 0 && up.X < Width) res.Add(up);
        var left = new Coord(c.X - 1, c.Y);
        if (left.X >= 0 && left.X < Width && left.Y >= 0 && left.Y < Height) res.Add(left);
        var down = new Coord(c.X, c.Y + 1);
        if (down.Y >= 0 && down.Y < Height && down.X >= 0 && down.X < Width) res.Add(down);
        var right = new Coord(c.X + 1, c.Y);
        if (right.X >= 0 && right.X < Width && right.Y >= 0 && right.Y < Height) res.Add(right);
        return res;
    }
}

class HeapNode { public Info Info; public int Priority; }

public class MinHeap
{
    private List<HeapNode> heap = new List<HeapNode>();
    public bool Any => heap.Count > 0;
    public void Enqueue(Info info, int priority)
    {
        var node = new HeapNode { Info = info, Priority = priority };
        heap.Add(node);
        int i = heap.Count - 1;
        while (i > 0)
        {
            int p = (i - 1) >> 1;
            if (heap[p].Priority <= heap[i].Priority) break;
            var tmp = heap[i];
            heap[i] = heap[p];
            heap[p] = tmp;
            i = p;
        }
    }
    public Info Dequeue()
    {
        var res = heap[0].Info;
        var last = heap[heap.Count - 1];
        heap[0] = last;
        heap.RemoveAt(heap.Count - 1);
        int i = 0;
        while (true)
        {
            int l = i * 2 + 1;
            if (l >= heap.Count) break;
            int r = l + 1;
            int smallest = l;
            if (r < heap.Count && heap[r].Priority < heap[l].Priority) smallest = r;
            if (heap[i].Priority <= heap[smallest].Priority) break;
            var tmp = heap[i];
            heap[i] = heap[smallest];
            heap[smallest] = tmp;
            i = smallest;
        }
        return res;
    }
}

public static class Solver
{
    static int Heuristic(Coord a, Coord b)
    {
        return Math.Abs(a.X - b.X) + Math.Abs(a.Y - b.Y);
    }

    public static int AStarConstrained(Grid grid, Coord start, Coord goal, int minStraight, int maxStraight)
    {
        var startInfo = new Info(start, new Coord(0, 0), 0);
        var frontier = new MinHeap();
        frontier.Enqueue(startInfo, 0);
        var cameFrom = new Dictionary<Info, Info>();
        var costSoFar = new Dictionary<Info, int>();
        cameFrom[startInfo] = startInfo;
        costSoFar[startInfo] = 0;

        while (frontier.Any)
        {
            var current = frontier.Dequeue();
            int currentCost = costSoFar[current];

            if (current.Coord.Equals(goal)) return currentCost;

            foreach (var next in grid.Neighbors4(current.Coord))
            {
                var newDir = next - current.Coord;
                var newNumStraight = (newDir == current.Dir) ? current.NumStraight + 1 : 1;
                var nextInfo = new Info(next, newDir, newNumStraight);
                int newCost = currentCost + grid.Data[next];

                bool isLowerCost = !costSoFar.ContainsKey(nextInfo) || newCost < costSoFar[nextInfo];
                bool isValidStraight = (current.NumStraight >= minStraight || newDir == current.Dir || current.Coord.Equals(start)) && (newNumStraight <= maxStraight);
                bool isNotOppositeDirection = newDir != current.Dir.Opposite;

                if (isLowerCost && isValidStraight && isNotOppositeDirection)
                {
                    costSoFar[nextInfo] = newCost;
                    cameFrom[nextInfo] = current;
                    int priority = newCost + Heuristic(next, goal);
                    frontier.Enqueue(nextInfo, priority);
                }
            }
        }
        return -1;
    }

    public static int SolveFromLines(List<string> input)
    {
        int height = input.Count;
        int width = input[0].Length;
        var data = new Dictionary<Coord, int>();
        for (int y = 0; y < height; y++)
        {
            var line = input[y];
            for (int x = 0; x < width; x++)
            {
                int val = line[x] - '0';
                data[new Coord(x, y)] = val;
            }
        }
        var grid = new Grid(width, height, data);
        var start = new Coord(0, 0);
        var goal = new Coord(width - 1, height - 1);
        return AStarConstrained(grid, start, goal, 4, 10);
    }
}

class Program
{
    static void Main(string[] args)
    {
        var lines = File.ReadAllLines("input.txt")
                        .Where(l => l != null && l.Trim().Length > 0)
                        .ToList();
        int ans = Solver.SolveFromLines(lines);
        Console.WriteLine(ans);
    }
}