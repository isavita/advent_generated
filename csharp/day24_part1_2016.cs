
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    const int MaxPois = 10;
    const int INF = int.MaxValue / 4;

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        if (lines.Length == 0) return;

        int rows = lines.Length;
        int cols = 0;
        foreach (var l in lines) if (l.Length > cols) cols = l.Length;

        var grid = new char[rows][];
        for (int r = 0; r < rows; r++)
        {
            var row = new char[cols];
            var line = lines[r];
            for (int c = 0; c < cols; c++)
                row[c] = c < line.Length ? line[c] : '.';
            grid[r] = row;
        }

        var poi = new (int r, int c)[MaxPois];
        int numPois = 0;
        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
                if (char.IsDigit(grid[r][c]))
                {
                    int idx = grid[r][c] - '0';
                    poi[idx] = (r, c);
                    if (idx >= numPois) numPois = idx + 1;
                }

        if (numPois == 0) { Console.WriteLine(0); return; }

        var graph = new int[numPois, numPois];
        for (int i = 0; i < numPois; i++)
            for (int j = 0; j < numPois; j++)
                graph[i, j] = INF;

        for (int i = 0; i < numPois; i++)
        {
            var d = Bfs(grid, rows, cols, poi[i].r, poi[i].c, numPois);
            for (int j = 0; j < numPois; j++) graph[i, j] = d[j];
        }

        var memo = new int[numPois, 1 << MaxPois];
        for (int i = 0; i < numPois; i++)
            for (int m = 0; m < (1 << numPois); m++)
                memo[i, m] = -1;

        int ans = Dp(0, 1, graph, numPois, memo);
        Console.WriteLine(ans);
    }

    static int[] Bfs(char[][] g, int R, int C, int sr, int sc, int n)
    {
        var dist = new int[n];
        for (int i = 0; i < n; i++) dist[i] = INF;

        var visited = new bool[R, C];
        var q = new Queue<(int r, int c, int d)>();
        visited[sr, sc] = true;
        q.Enqueue((sr, sc, 0));

        while (q.Count > 0)
        {
            var (r, c, d) = q.Dequeue();

            if (char.IsDigit(g[r][c]))
            {
                int idx = g[r][c] - '0';
                if (dist[idx] == INF) dist[idx] = d;
            }

            int[] dr = { -1, 1, 0, 0 };
            int[] dc = { 0, 0, -1, 1 };
            for (int k = 0; k < 4; k++)
            {
                int nr = r + dr[k], nc = c + dc[k];
                if (nr < 0 || nr >= R || nc < 0 || nc >= C) continue;
                if (g[nr][nc] == '#') continue;
                if (visited[nr, nc]) continue;
                visited[nr, nc] = true;
                q.Enqueue((nr, nc, d + 1));
            }
        }
        return dist;
    }

    static int Dp(int cur, int mask, int[,] g, int n, int[,] memo)
    {
        if (mask == (1 << n) - 1) return 0;
        if (memo[cur, mask] != -1) return memo[cur, mask];

        int best = INF;
        for (int nxt = 0; nxt < n; nxt++)
        {
            if ((mask & (1 << nxt)) != 0) continue;
            int w = g[cur, nxt];
            if (w == INF) continue;
            int cand = w + Dp(nxt, mask | (1 << nxt), g, n, memo);
            if (cand < best) best = cand;
        }
        memo[cur, mask] = best;
        return best;
    }
}
