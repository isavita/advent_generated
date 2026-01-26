
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Solution
{
    private static readonly int[] DR = { 1, -1, 0, 0 };
    private static readonly int[] DC = { 0, 0, 1, -1 };

    public static void Main(string[] args)
    {
        var lines = File.ReadAllLines("input.txt");
        var nr = lines.Length;
        var nc = lines[0].Length;
        var grid = new int[nr, nc];

        for (var r = 0; r < nr; r++)
        {
            for (var c = 0; c < nc; c++)
            {
                grid[r, c] = lines[r][c] - '0';
            }
        }

        var sumScores = 0;
        for (var r = 0; r < nr; r++)
        {
            for (var c = 0; c < nc; c++)
            {
                if (grid[r, c] == 0)
                {
                    sumScores += Bfs(grid, r, c, nr, nc);
                }
            }
        }
        Console.WriteLine(sumScores);
    }

    private static int Bfs(int[,] grid, int sr, int sc, int nr, int nc)
    {
        var reached = new HashSet<(int, int)>();
        var front = new Queue<(int, int, int)>();
        var visited = new HashSet<(int, int, int)>();

        front.Enqueue((sr, sc, 0));

        while (front.Count > 0)
        {
            var (r, c, h) = front.Dequeue();

            if (h == 9)
            {
                reached.Add((r, c));
                continue;
            }

            for (var i = 0; i < 4; i++)
            {
                var nr2 = r + DR[i];
                var nc2 = c + DC[i];

                if (nr2 >= 0 && nr2 < nr && nc2 >= 0 && nc2 < nc)
                {
                    if (grid[nr2, nc2] == h + 1)
                    {
                        var next = (nr2, nc2, h + 1);
                        if (visited.Add(next))
                        {
                            front.Enqueue(next);
                        }
                    }
                }
            }
        }
        return reached.Count;
    }
}
