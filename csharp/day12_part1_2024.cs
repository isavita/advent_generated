
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static int rows, cols;
    static char[,] grid;
    static bool[,] visited;

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        rows = lines.Length;
        cols = lines[0].Length;
        grid = new char[rows, cols];
        visited = new bool[rows, cols];
        for (int i = 0; i < rows; i++)
        {
            var line = lines[i];
            for (int j = 0; j < cols; j++)
                grid[i, j] = line[j];
        }

        long total = 0;
        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
                if (!visited[r, c])
                {
                    var res = Region(r, c);
                    total += res.area * res.perimeter;
                }

        Console.WriteLine(total);
    }

    static (long area, long perimeter) Region(int sr, int sc)
    {
        char ch = grid[sr, sc];
        long area = 0, perimeter = 0;
        var q = new Queue<(int r, int c)>();
        q.Enqueue((sr, sc));
        visited[sr, sc] = true;
        int[] dr = { 0, 0, 1, -1 };
        int[] dc = { 1, -1, 0, 0 };

        while (q.Count > 0)
        {
            var (r, c) = q.Dequeue();
            area++;
            for (int k = 0; k < 4; k++)
            {
                int nr = r + dr[k], nc = c + dc[k];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols)
                {
                    if (grid[nr, nc] != ch) perimeter++;
                    else if (!visited[nr, nc])
                    {
                        visited[nr, nc] = true;
                        q.Enqueue((nr, nc));
                    }
                }
                else perimeter++;
            }
        }
        return (area, perimeter);
    }
}
