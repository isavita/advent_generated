
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    const int MAX_H = 1005;
    const int MAX_W = 1005;
    const int MAX_STEPS = 20;
    const int INF = -1;

    static char[,] grid = new char[MAX_H, MAX_W];
    static int[,] walls = new int[MAX_H, MAX_W];
    static int[,] dist_s = new int[MAX_H, MAX_W];
    static int[,] dist_e = new int[MAX_H, MAX_W];
    static int[,] dist_c = new int[MAX_H, MAX_W];
    static int H, W;
    static (int r, int c) S, E;

    static readonly int[] dr = { 1, -1, 0, 0 };
    static readonly int[] dc = { 0, 0, 1, -1 };

    static bool IsValid(int r, int c) => r >= 0 && r < H && c >= 0 && c < W && walls[r, c] == 0;

    static void Bfs((int r, int c) start, int[,] dist)
    {
        for (int i = 0; i < H; i++)
            for (int j = 0; j < W; j++)
                dist[i, j] = INF;

        if (!IsValid(start.r, start.c)) return;

        var q = new Queue<(int, int)>();
        dist[start.r, start.c] = 0;
        q.Enqueue(start);

        while (q.Count > 0)
        {
            var (r, c) = q.Dequeue();
            int d = dist[r, c];

            for (int i = 0; i < 4; i++)
            {
                int nr = r + dr[i], nc = c + dc[i];
                if (IsValid(nr, nc) && dist[nr, nc] == INF)
                {
                    dist[nr, nc] = d + 1;
                    q.Enqueue((nr, nc));
                }
            }
        }
    }

    static void LimitedBfs((int r, int c) start, int maxSteps)
    {
        for (int i = 0; i < H; i++)
            for (int j = 0; j < W; j++)
                dist_c[i, j] = INF;

        var q = new Queue<(int, int)>();
        dist_c[start.r, start.c] = 0;
        q.Enqueue(start);

        while (q.Count > 0)
        {
            var (r, c) = q.Dequeue();
            int d = dist_c[r, c];
            if (d >= maxSteps) continue;

            for (int i = 0; i < 4; i++)
            {
                int nr = r + dr[i], nc = c + dc[i];
                if (nr >= 0 && nr < H && nc >= 0 && nc < W && dist_c[nr, nc] == INF)
                {
                    dist_c[nr, nc] = d + 1;
                    q.Enqueue((nr, nc));
                }
            }
        }
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        H = lines.Length;
        W = lines[0].Length;

        for (int i = 0; i < H; i++)
        {
            for (int j = 0; j < W; j++)
            {
                grid[i, j] = lines[i][j];
                if (grid[i, j] == 'S') { S = (i, j); }
                else if (grid[i, j] == 'E') { E = (i, j); }
                walls[i, j] = grid[i, j] == '#' ? 1 : 0;
            }
        }

        Bfs(S, dist_s);
        Bfs(E, dist_e);

        int normalCost = dist_s[E.r, E.c];
        if (normalCost == INF) { Console.WriteLine(0); return; }

        long cheatCount = 0;

        for (int sr = 0; sr < H; sr++)
        {
            for (int sc = 0; sc < W; sc++)
            {
                if (walls[sr, sc] == 1 || dist_s[sr, sc] == INF) continue;
                int sd = dist_s[sr, sc];
                LimitedBfs((sr, sc), MAX_STEPS);

                for (int er = 0; er < H; er++)
                {
                    for (int ec = 0; ec < W; ec++)
                    {
                        if (walls[er, ec] == 1 || dist_e[er, ec] == INF) continue;
                        int s = dist_c[er, ec];
                        if (s > 0 && s <= MAX_STEPS)
                        {
                            int ed = dist_e[er, ec];
                            int totalCost = sd + s + ed;
                            if (totalCost < normalCost && normalCost - totalCost >= 100)
                                cheatCount++;
                        }
                    }
                }
            }
        }

        Console.WriteLine(cheatCount);
    }
}
