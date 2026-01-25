
using System;
using System.IO;

class Program
{
    static int[,] grid;
    static long[,] dp;
    static int rows, cols;
    static readonly int[] dr = { 1, -1, 0, 0 };
    static readonly int[] dc = { 0, 0, 1, -1 };

    static long Dfs(int r, int c)
    {
        long val = dp[r, c];
        if (val != -1) return val;
        int h = grid[r, c];
        if (h == 9) return dp[r, c] = 1;
        long sum = 0;
        for (int k = 0; k < 4; k++)
        {
            int nr = r + dr[k], nc = c + dc[k];
            if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) continue;
            if (grid[nr, nc] == h + 1) sum += Dfs(nr, nc);
        }
        return dp[r, c] = sum;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        rows = lines.Length;
        if (rows == 0) { Console.WriteLine(0); return; }
        cols = lines[0].Length;
        grid = new int[rows, cols];
        dp = new long[rows, cols];

        for (int i = 0; i < rows; i++)
        {
            string line = lines[i];
            for (int j = 0; j < cols; j++)
            {
                grid[i, j] = line[j] - '0';
                dp[i, j] = -1;
            }
        }

        long total = 0;
        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
                if (grid[r, c] == 0)
                    total += Dfs(r, c);

        Console.WriteLine(total);
    }
}
