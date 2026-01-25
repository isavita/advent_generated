using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        if (!File.Exists("input.txt")) return;
        var lines = File.ReadAllLines("input.txt");
        int R = lines.Length;
        if (R == 0) { Console.WriteLine("Total rolls removed: 0"); return; }
        int C = lines.Max(l => l.Length);
        if (C == 0) { Console.WriteLine("Total rolls removed: 0"); return; }

        char[][] grid = new char[R][];
        for (int i = 0; i < R; i++)
        {
            grid[i] = new char[C];
            var src = lines[i];
            for (int j = 0; j < src.Length; j++) grid[i][j] = src[j];
        }

        int removed = 0, changed;
        int[] dr = { -1, -1, -1, 0, 0, 1, 1, 1 };
        int[] dc = { -1, 0, 1, -1, 1, -1, 0, 1 };

        do
        {
            changed = 0;
            for (int r = 0; r < R; r++)
                for (int c = 0; c < C; c++)
                    if (grid[r][c] == '@')
                    {
                        int cnt = 0;
                        for (int k = 0; k < 8; k++)
                        {
                            int nr = r + dr[k], nc = c + dc[k];
                            if (nr >= 0 && nr < R && nc >= 0 && nc < C && grid[nr][nc] == '@') cnt++;
                        }
                        if (cnt < 4) { grid[r][c] = '*'; changed++; }
                    }

            for (int r = 0; r < R; r++)
                for (int c = 0; c < C; c++)
                    if (grid[r][c] == '*')
                    {
                        grid[r][c] = '.';
                        removed++;
                    }
        } while (changed != 0);

        Console.WriteLine($"Total rolls removed: {removed}");
    }
}
