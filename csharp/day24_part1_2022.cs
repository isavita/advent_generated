
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    struct Point
    {
        public int X, Y;
        public Point(int x, int y) { X = x; Y = y; }
    }

    static int[] dx = { 0, 0, 1, -1, 0 };
    static int[] dy = { 1, -1, 0, 0, 0 };
    static char[] bliz = { '^', '>', 'v', '<' };

    static int Steps(char[][] grid, int rows, int cols, Point start, Point end, int initialStep)
    {
        var q = new Queue<(Point pos, int step)>();
        q.Enqueue((start, initialStep));
        bool[,,] seen = new bool[rows, cols, rows * cols];
        seen[start.Y, start.X, initialStep % (rows * cols)] = true;

        while (q.Count > 0)
        {
            var (pos, step) = q.Dequeue();
            if (pos.X == end.X && pos.Y == end.Y) return step;

            for (int i = 0; i < 5; i++)
            {
                int nx = pos.X + dx[i], ny = pos.Y + dy[i], ns = step + 1;
                if (nx < 0 || nx >= cols || ny < 0 || ny >= rows || grid[ny][nx] == '#' || seen[ny, nx, ns % (rows * cols)]) continue;

                bool valid = true;
                if (ny > 0 && ny < rows - 1)
                {
                    for (int j = 0; j < 4; j++)
                    {
                        int px, py;
                        if (j == 0) { px = nx; py = (ny + ns) % (rows - 2); if (py == 0) py = rows - 2; }
                        else if (j == 1) { px = (nx - ns) % (cols - 2); if (px < 0) px += cols - 2; if (px == 0) px = cols - 2; py = ny; }
                        else if (j == 2) { px = nx; py = (ny - ns) % (rows - 2); if (py < 0) py += rows - 2; if (py == 0) py = rows - 2; }
                        else { px = (nx + ns) % (cols - 2); if (px == 0) px = cols - 2; py = ny; }

                        if (grid[py][px] == bliz[j]) { valid = false; break; }
                    }
                }
                if (!valid) continue;

                seen[ny, nx, ns % (rows * cols)] = true;
                q.Enqueue((new Point(nx, ny), ns));
            }
        }
        return -1;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int rows = lines.Length, cols = lines[0].Length;
        var grid = new char[rows][];
        for (int i = 0; i < rows; i++) grid[i] = lines[i].ToCharArray();

        Point entrance = new Point(1, 0);
        Point exit = new Point(cols - 2, rows - 1);
        Console.WriteLine(Steps(grid, rows, cols, entrance, exit, 0));
    }
}
