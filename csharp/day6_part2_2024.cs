using System;
using System.IO;

class Program
{
    const int MaxSteps = 2000000;
    static readonly (int dx, int dy)[] dirs = { (0, -1), (1, 0), (0, 1), (-1, 0) };

    static bool Loops(char[][] grid, int sx, int sy, int sdir, int h, int w, bool[] seen)
    {
        int x = sx, y = sy, dir = sdir;
        int limit = h * w * 4;
        Array.Clear(seen, 0, limit);

        while (true)
        {
            int idx = (y * w + x) * 4 + dir;
            if (seen[idx]) return true;
            seen[idx] = true;

            int nx = x + dirs[dir].dx;
            int ny = y + dirs[dir].dy;

            if (nx < 0 || nx >= w || ny < 0 || ny >= h) return false;
            if (grid[ny][nx] == '#')
            {
                dir = (dir + 1) & 3;
                continue;
            }

            x = nx;
            y = ny;
        }
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int h = lines.Length;
        int w = lines[0].Length;
        char[][] grid = new char[h][];
        for (int i = 0; i < h; i++)
        {
            grid[i] = lines[i].ToCharArray();
        }

        int startX = 0, startY = 0, startDir = 0;
        for (int i = 0; i < h; i++)
        {
            for (int j = 0; j < w; j++)
            {
                switch (grid[i][j])
                {
                    case '^': startX = j; startY = i; startDir = 0; break;
                    case '>': startX = j; startY = i; startDir = 1; break;
                    case 'v': startX = j; startY = i; startDir = 2; break;
                    case '<': startX = j; startY = i; startDir = 3; break;
                }
            }
        }
        grid[startY][startX] = '.';

        bool[] seen = new bool[h * w * 4];
        int count = 0;
        for (int y = 0; y < h; y++)
        {
            for (int x = 0; x < w; x++)
            {
                if (x == startX && y == startY) continue;
                if (grid[y][x] != '.') continue;

                grid[y][x] = '#';
                if (Loops(grid, startX, startY, startDir, h, w, seen))
                {
                    count++;
                }
                grid[y][x] = '.';
            }
        }

        Console.WriteLine(count);
    }
}
