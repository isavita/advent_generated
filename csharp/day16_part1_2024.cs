
using System;
using System.IO;
using System.Collections.Generic;

class Node
{
    public int x, y, d, cost;
    public Node(int x, int y, int d, int cost)
    {
        this.x = x; this.y = y; this.d = d; this.cost = cost;
    }
}

class MinHeap
{
    private List<Node> a = new List<Node>();
    public bool Any => a.Count > 0;

    public void Enqueue(Node n)
    {
        a.Add(n);
        SiftUp(a.Count - 1);
    }

    public Node Dequeue()
    {
        Node res = a[0];
        int last = a.Count - 1;
        a[0] = a[last];
        a.RemoveAt(last);
        if (a.Count > 0) SiftDown(0);
        return res;
    }

    private void SiftUp(int i)
    {
        while (i > 0)
        {
            int p = (i - 1) / 2;
            if (a[i].cost < a[p].cost)
            {
                Swap(i, p);
                i = p;
            }
            else break;
        }
    }

    private void SiftDown(int i)
    {
        int n = a.Count;
        while (true)
        {
            int l = 2 * i + 1;
            int r = l + 1;
            int smallest = i;
            if (l < n && a[l].cost < a[smallest].cost) smallest = l;
            if (r < n && a[r].cost < a[smallest].cost) smallest = r;
            if (smallest == i) break;
            Swap(i, smallest);
            i = smallest;
        }
    }

    private void Swap(int i, int j)
    {
        var t = a[i];
        a[i] = a[j];
        a[j] = t;
    }
}

class Program
{
    static void Main(string[] args)
    {
        var grid = File.ReadAllLines("input.txt");
        int n = grid.Length;
        if (n == 0)
        {
            Console.WriteLine(0);
            return;
        }
        int m = grid[0].Length;

        int sx = 0, sy = 0, ex = 0, ey = 0;
        for (int i = 0; i < n; i++)
        {
            for (int j = 0; j < m; j++)
            {
                char c = grid[i][j];
                if (c == 'S')
                {
                    sx = i; sy = j;
                }
                else if (c == 'E')
                {
                    ex = i; ey = j;
                }
            }
        }

        int[] dx = new int[] { -1, 0, 1, 0 };
        int[] dy = new int[] { 0, 1, 0, -1 };

        int[,,] dist = new int[n, m, 4];
        for (int i = 0; i < n; i++)
            for (int j = 0; j < m; j++)
                for (int d = 0; d < 4; d++)
                    dist[i, j, d] = int.MaxValue;

        dist[sx, sy, 1] = 0;

        var heap = new MinHeap();
        heap.Enqueue(new Node(sx, sy, 1, 0));

        while (heap.Any)
        {
            var u = heap.Dequeue();
            if (dist[u.x, u.y, u.d] < u.cost) continue;
            if (u.x == ex && u.y == ey)
            {
                Console.WriteLine(u.cost);
                return;
            }

            int right = (u.d + 1) & 3;
            int left = (u.d + 3) & 3;
            int[] ndirs = new int[] { right, left };
            foreach (int nd in ndirs)
            {
                int nc = u.cost + 1000;
                if (nc < dist[u.x, u.y, nd])
                {
                    dist[u.x, u.y, nd] = nc;
                    heap.Enqueue(new Node(u.x, u.y, nd, nc));
                }
            }

            int nx = u.x + dx[u.d];
            int ny = u.y + dy[u.d];
            if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#')
            {
                int nc = u.cost + 1;
                if (nc < dist[nx, ny, u.d])
                {
                    dist[nx, ny, u.d] = nc;
                    heap.Enqueue(new Node(nx, ny, u.d, nc));
                }
            }
        }

        Console.WriteLine(-1);
    }
}
