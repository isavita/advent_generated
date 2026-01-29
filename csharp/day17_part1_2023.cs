using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    const int MIN_STRAIGHT = 0;
    const int MAX_STRAIGHT = 3;

    struct Coord
    {
        public int X;
        public int Y;
        public Coord(int x, int y) { X = x; Y = y; }
    }

    struct State
    {
        public Coord Coord;
        public Coord Dir;
        public int NumStraight;
        public State(Coord c, Coord d, int n) { Coord = c; Dir = d; NumStraight = n; }
    }

    class HeapNode
    {
        public int Priority;
        public int Cost;
        public State State;
        public HeapNode(int p, int c, State s) { Priority = p; Cost = c; State = s; }
    }

    class PriorityQueue
    {
        List<HeapNode> heap = new List<HeapNode>();
        public int Count => heap.Count;
        public void Push(HeapNode node)
        {
            heap.Add(node);
            int i = heap.Count - 1;
            while (i > 0)
            {
                int p = (i - 1) / 2;
                if (heap[i].Priority < heap[p].Priority)
                {
                    var t = heap[i]; heap[i] = heap[p]; heap[p] = t;
                    i = p;
                }
                else break;
            }
        }
        public HeapNode Pop()
        {
            var root = heap[0];
            var last = heap[heap.Count - 1];
            heap.RemoveAt(heap.Count - 1);
            if (heap.Count > 0)
            {
                heap[0] = last;
                int i = 0;
                while (true)
                {
                    int l = 2 * i + 1;
                    int r = 2 * i + 2;
                    int s = i;
                    if (l < heap.Count && heap[l].Priority < heap[s].Priority) s = l;
                    if (r < heap.Count && heap[r].Priority < heap[s].Priority) s = r;
                    if (s != i)
                    {
                        var t = heap[i]; heap[i] = heap[s]; heap[s] = t;
                        i = s;
                    }
                    else break;
                }
            }
            return root;
        }
        public bool IsEmpty() => heap.Count == 0;
    }

    static int Heuristic(Coord a, Coord b) => Math.Abs(a.X - b.X) + Math.Abs(a.Y - b.Y);
    static int DirToIndex(Coord dir)
    {
        if (dir.Y == -1) return 0; // Up
        if (dir.X == -1) return 1; // Left
        if (dir.Y == 1)  return 2; // Down
        if (dir.X == 1)  return 3; // Right
        return 4; // Start/None
    }
    static bool Equal(Coord a, Coord b) => a.X == b.X && a.Y == b.Y;
    static Coord Opposite(Coord d) => new Coord(-d.X, -d.Y);

    static int AStar(int[,] grid, Coord start, Coord goal)
    {
        int height = grid.GetLength(0);
        int width = grid.GetLength(1);
        Coord[] directions = new Coord[] { new Coord(0, -1), new Coord(-1, 0), new Coord(0, 1), new Coord(1, 0) };

        int[,,,] cost = new int[height, width, 5, MAX_STRAIGHT + 1];
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                for (int d = 0; d < 5; d++)
                    for (int s = 0; s <= MAX_STRAIGHT; s++)
                        cost[y, x, d, s] = int.MaxValue;

        var frontier = new PriorityQueue();
        var startState = new State(start, new Coord(0, 0), 0);
        int startDirIdx = DirToIndex(startState.Dir);
        cost[start.Y, start.X, startDirIdx, 0] = 0;
        frontier.Push(new HeapNode(Heuristic(start, goal), 0, startState));

        int minFinalCost = int.MaxValue;

        while (!frontier.IsEmpty())
        {
            var node = frontier.Pop();
            var cur = node.State;
            int currentCost = node.Cost;
            int cx = cur.Coord.X;
            int cy = cur.Coord.Y;
            int dirIdx = DirToIndex(cur.Dir);

            if (currentCost > cost[cy, cx, dirIdx, cur.NumStraight]) continue;
            if (Equal(cur.Coord, goal))
            {
                if (currentCost < minFinalCost) minFinalCost = currentCost;
            }

            if (currentCost >= minFinalCost) continue;

            foreach (var d in directions)
            {
                int nx = cx + d.X;
                int ny = cy + d.Y;
                if (nx < 0 || ny < 0 || nx >= width || ny >= height) continue;

                if (!Equal(cur.Dir, new Coord(0, 0)) && Equal(d, Opposite(cur.Dir))) continue;

                int newStraight = Equal(d, cur.Dir) ? cur.NumStraight + 1 : 1;
                int newDirIdx = DirToIndex(d);
                if (newStraight > MAX_STRAIGHT) continue;

                if (!Equal(cur.Dir, new Coord(0, 0)) && !Equal(d, cur.Dir) && cur.NumStraight < MIN_STRAIGHT) continue;

                int newCost = currentCost + grid[ny, nx];
                if (newCost < cost[ny, nx, newDirIdx, newStraight])
                {
                    cost[ny, nx, newDirIdx, newStraight] = newCost;
                    int priority = newCost + Heuristic(new Coord(nx, ny), goal);
                    var nextState = new State(new Coord(nx, ny), d, newStraight);
                    frontier.Push(new HeapNode(priority, newCost, nextState));
                }
            }
        }

        return (minFinalCost == int.MaxValue) ? -1 : minFinalCost;
    }

    static void Main()
    {
        var raw = File.ReadAllLines("input.txt");
        var lines = new List<string>();
        foreach (var l in raw)
        {
            if (string.IsNullOrWhiteSpace(l)) continue;
            lines.Add(l.Trim());
        }

        int height = lines.Count;
        if (height == 0)
        {
            Console.WriteLine(-1);
            return;
        }
        int width = lines[0].Length;
        for (int i = 1; i < height; i++)
            if (lines[i].Length != width)
            {
                Console.WriteLine(-1);
                return;
            }

        int[,] grid = new int[height, width];
        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
                grid[y, x] = lines[y][x] - '0';
        }

        Coord start = new Coord(0, 0);
        Coord goal = new Coord(width - 1, height - 1);
        int result = AStar(grid, start, goal);
        Console.WriteLine(result);
    }
}