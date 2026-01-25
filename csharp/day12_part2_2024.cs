using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    struct Point { public int R, C; public Point(int r, int c) { R = r; C = c; } }

    const int MaxDim = 200;
    const int Pad = 1;
    const string InputFile = "input.txt";

    static void Main()
    {
        var lines = File.ReadAllLines(InputFile);
        int h = lines.Length;
        int w = lines[0].Length;
        int dim = h + 2 * Pad;
        int wdim = w + 2 * Pad;
        char[,] grid = new char[dim, wdim];
        bool[,] visited = new bool[dim, wdim];
        bool[,] inRegion = new bool[dim, wdim];

        for (int r = Pad; r < Pad + h; r++)
            for (int c = Pad; c < Pad + w; c++)
                grid[r, c] = lines[r - Pad][c - Pad];

        long total1 = 0, total2 = 0;
        int[] dr = { -1, 1, 0, 0 };
        int[] dc = { 0, 0, -1, 1 };

        var queue = new List<Point>();

        for (int r = Pad; r < Pad + h; r++)
            for (int c = Pad; c < Pad + w; c++)
                if (!visited[r, c])
                {
                    char type = grid[r, c];
                    long area = 0, perimeter = 0;
                    queue.Clear();
                    queue.Add(new Point(r, c));
                    visited[r, c] = true;
                    int qi = 0;
                    while (qi < queue.Count)
                    {
                        var p = queue[qi++];
                        area++;
                        for (int i = 0; i < 4; i++)
                        {
                            int nr = p.R + dr[i], nc = p.C + dc[i];
                            if (grid[nr, nc] != type) perimeter++;
                            else if (!visited[nr, nc])
                            {
                                visited[nr, nc] = true;
                                queue.Add(new Point(nr, nc));
                            }
                        }
                    }

                    total1 += area * perimeter;

                    long top = 0, bottom = 0, left = 0, right = 0;
                    long topAdj = 0, bottomAdj = 0, leftAdj = 0, rightAdj = 0;

                    Array.Clear(inRegion, 0, inRegion.Length);
                    foreach (var p in queue) inRegion[p.R, p.C] = true;

                    foreach (var p in queue)
                    {
                        if (grid[p.R - 1, p.C] != type) top++;
                        if (grid[p.R + 1, p.C] != type) bottom++;
                        if (grid[p.R, p.C - 1] != type) left++;
                        if (grid[p.R, p.C + 1] != type) right++;

                        if (inRegion[p.R, p.C + 1])
                        {
                            if (grid[p.R - 1, p.C] != type && grid[p.R - 1, p.C + 1] != type) topAdj++;
                            if (grid[p.R + 1, p.C] != type && grid[p.R + 1, p.C + 1] != type) bottomAdj++;
                        }
                        if (inRegion[p.R + 1, p.C])
                        {
                            if (grid[p.R, p.C - 1] != type && grid[p.R + 1, p.C - 1] != type) leftAdj++;
                            if (grid[p.R, p.C + 1] != type && grid[p.R + 1, p.C + 1] != type) rightAdj++;
                        }
                    }

                    long sides = (top - topAdj) + (bottom - bottomAdj) + (left - leftAdj) + (right - rightAdj);
                    total2 += area * sides;
                }

        Console.WriteLine($"Part 1 Total Price: {total1}");
        Console.WriteLine($"Part 2 Total Price: {total2}");
    }
}
