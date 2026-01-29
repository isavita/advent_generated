using System;
using System.Collections.Generic;
using System.IO;

class Node
{
    public int X;
    public int Y;
    public int Risk;
    public Node(int x, int y, int risk) { X = x; Y = y; Risk = risk; }
}

class MinHeap
{
    private List<Node> data = new List<Node>();
    public int Count => data.Count;

    private void Swap(int i, int j)
    {
        var t = data[i];
        data[i] = data[j];
        data[j] = t;
    }

    public void Push(Node n)
    {
        data.Add(n);
        int i = data.Count - 1;
        while (i > 0)
        {
            int p = (i - 1) / 2;
            if (data[i].Risk >= data[p].Risk) break;
            Swap(i, p);
            i = p;
        }
    }

    public Node Pop()
    {
        var min = data[0];
        var last = data[data.Count - 1];
        data.RemoveAt(data.Count - 1);
        if (data.Count > 0)
        {
            data[0] = last;
            int i = 0;
            while (true)
            {
                int left = i * 2 + 1;
                int right = left + 1;
                int smallest = i;
                if (left < data.Count && data[left].Risk < data[smallest].Risk) smallest = left;
                if (right < data.Count && data[right].Risk < data[smallest].Risk) smallest = right;
                if (smallest == i) break;
                Swap(i, smallest);
                i = smallest;
            }
        }
        return min;
    }
}

class Program
{
    static int Dijkstra(int[,] grid)
    {
        int rows = grid.GetLength(0);
        int cols = grid.GetLength(1);
        var dist = new int[rows, cols];
        for (int i = 0; i < rows; i++)
            for (int j = 0; j < cols; j++)
                dist[i, j] = int.MaxValue;
        dist[0, 0] = 0;

        var heap = new MinHeap();
        heap.Push(new Node(0, 0, 0));

        int[] dx = new int[] { 1, 0, -1, 0 };
        int[] dy = new int[] { 0, 1, 0, -1 };

        while (heap.Count > 0)
        {
            var cur = heap.Pop();
            int x = cur.X, y = cur.Y, risk = cur.Risk;
            if (risk != dist[x, y]) continue;
            if (x == rows - 1 && y == cols - 1) return risk;

            for (int dir = 0; dir < 4; dir++)
            {
                int nx = x + dx[dir];
                int ny = y + dy[dir];
                if (nx >= 0 && ny >= 0 && nx < rows && ny < cols)
                {
                    int nextRisk = risk + grid[nx, ny];
                    if (nextRisk < dist[nx, ny])
                    {
                        dist[nx, ny] = nextRisk;
                        heap.Push(new Node(nx, ny, nextRisk));
                    }
                }
            }
        }

        return -1;
    }

    static void Main(string[] args)
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("Error opening file");
            return;
        }

        var lines = File.ReadAllLines("input.txt");
        var gridList = new List<List<int>>();
        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            var row = new List<int>();
            foreach (char c in line)
            {
                if (c >= '0' && c <= '9') row.Add(c - '0');
            }
            if (row.Count > 0) gridList.Add(row);
        }

        int rows = gridList.Count;
        if (rows == 0)
        {
            Console.WriteLine(0);
            return;
        }
        int cols = gridList[0].Count;
        var grid = new int[rows, cols];
        for (int i = 0; i < rows; i++)
            for (int j = 0; j < cols; j++)
                grid[i, j] = gridList[i][j];

        int answer = Dijkstra(grid);
        Console.WriteLine(answer);
    }
}