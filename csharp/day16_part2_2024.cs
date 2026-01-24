
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    const int MAX = 150;
    static readonly int[] dx = { -1, 0, 1, 0 };
    static readonly int[] dy = { 0, 1, 0, -1 };

    struct State : IComparable<State>
    {
        public int cost, x, y, d;
        public int CompareTo(State other) => cost.CompareTo(other.cost);
    }

    struct PointDir
    {
        public int x, y, d;
    }

    static char[,] grid = new char[MAX, MAX];
    static int n, m, sx, sy, ex, ey;
    static int[,,] dist = new int[MAX, MAX, 4];
    static bool[,,] vis = new bool[MAX, MAX, 4];
    static bool[,] used = new bool[MAX, MAX];

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        n = lines.Length;
        m = lines[0].Length;
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < m; j++)
            {
                grid[i, j] = lines[i][j];
                if (grid[i, j] == 'S') { sx = i; sy = j; }
                else if (grid[i, j] == 'E') { ex = i; ey = j; }
                for (int d = 0; d < 4; d++)
                    dist[i, j, d] = int.MaxValue;
            }
        }

        var pq = new PriorityQueue<State>();
        dist[sx, sy, 1] = 0;
        pq.Enqueue(new State { cost = 0, x = sx, y = sy, d = 1 });

        while (pq.Count > 0)
        {
            var cur = pq.Dequeue();
            int c = cur.cost, x = cur.x, y = cur.y, d = cur.d;
            if (c > dist[x, y, d]) continue;

            int nd = (d + 1) & 3, nc = c + 1000;
            if (nc < dist[x, y, nd])
            {
                dist[x, y, nd] = nc;
                pq.Enqueue(new State { cost = nc, x = x, y = y, d = nd });
            }
            nd = (d + 3) & 3;
            if (nc < dist[x, y, nd])
            {
                dist[x, y, nd] = nc;
                pq.Enqueue(new State { cost = nc, x = x, y = y, d = nd });
            }

            int nx = x + dx[d], ny = y + dy[d];
            if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx, ny] != '#')
            {
                nc = c + 1;
                if (nc < dist[nx, ny, d])
                {
                    dist[nx, ny, d] = nc;
                    pq.Enqueue(new State { cost = nc, x = nx, y = ny, d = d });
                }
            }
        }

        int best = int.MaxValue;
        for (int d = 0; d < 4; d++)
            best = Math.Min(best, dist[ex, ey, d]);
        if (best == int.MaxValue) { Console.WriteLine(0); return; }

        var stack = new Stack<PointDir>();
        for (int d = 0; d < 4; d++)
            if (dist[ex, ey, d] == best) stack.Push(new PointDir { x = ex, y = ey, d = d });

        while (stack.Count > 0)
        {
            var p = stack.Pop();
            int x = p.x, y = p.y, d = p.d;
            used[x, y] = true;
            int cu = dist[x, y, d];

            int pd = (d + 1) & 3;
            if (cu >= 1000 && dist[x, y, pd] == cu - 1000 && !vis[x, y, pd])
            {
                vis[x, y, pd] = true;
                stack.Push(new PointDir { x = x, y = y, d = pd });
            }
            pd = (d + 3) & 3;
            if (cu >= 1000 && dist[x, y, pd] == cu - 1000 && !vis[x, y, pd])
            {
                vis[x, y, pd] = true;
                stack.Push(new PointDir { x = x, y = y, d = pd });
            }

            int px = x - dx[d], py = y - dy[d];
            if (px >= 0 && px < n && py >= 0 && py < m && grid[px, py] != '#')
                if (cu > 0 && dist[px, py, d] == cu - 1 && !vis[px, py, d])
                {
                    vis[px, py, d] = true;
                    stack.Push(new PointDir { x = px, y = py, d = d });
                }
        }

        int count = 0;
        for (int i = 0; i < n; i++)
            for (int j = 0; j < m; j++)
                if (used[i, j]) count++;
        Console.WriteLine(count);
    }
}

class PriorityQueue<T> where T : IComparable<T>
{
    private List<T> heap = new List<T>();
    public int Count => heap.Count;
    public void Enqueue(T item)
    {
        heap.Add(item);
        int i = heap.Count - 1;
        while (i > 0)
        {
            int p = (i - 1) / 2;
            if (heap[p].CompareTo(item) <= 0) break;
            heap[i] = heap[p]; i = p;
        }
        heap[i] = item;
    }
    public T Dequeue()
    {
        T ret = heap[0], last = heap[heap.Count - 1];
        heap.RemoveAt(heap.Count - 1);
        if (heap.Count == 0) return ret;
        int i = 0;
        while (i < heap.Count / 2)
        {
            int l = 2 * i + 1, r = l + 1, c = r < heap.Count && heap[r].CompareTo(heap[l]) < 0 ? r : l;
            if (heap[c].CompareTo(last) >= 0) break;
            heap[i] = heap[c]; i = c;
        }
        heap[i] = last;
        return ret;
    }
}
