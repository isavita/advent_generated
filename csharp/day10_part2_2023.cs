
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        if (!File.Exists("input.txt")) return;
        var lines = File.ReadAllLines("input.txt");
        if (lines.Length == 0) return;

        int h = lines.Length, w = lines[0].Length;
        char[][] grid = lines.Select(l => l.ToCharArray()).ToArray();
        int sx = -1, sy = -1;

        for (int y = 0; y < h; y++)
        {
            for (int x = 0; x < w; x++)
            {
                if (grid[y][x] == 'S')
                {
                    sx = x; sy = y;
                    break;
                }
            }
            if (sx != -1) break;
        }

        bool up = sy > 0 && "|7F".Contains(grid[sy - 1][sx]);
        bool down = sy < h - 1 && "|LJ".Contains(grid[sy + 1][sx]);
        bool left = sx > 0 && "-LF".Contains(grid[sy][sx - 1]);
        bool right = sx < w - 1 && "-7J".Contains(grid[sy][sx + 1]);

        char sTile = up && down ? '|' : (left && right ? '-' : (up && right ? 'L' : (up && left ? 'J' : (down && left ? '7' : 'F'))));
        grid[sy][sx] = sTile;

        bool[][] isLoop = new bool[h][];
        for (int i = 0; i < h; i++) isLoop[i] = new bool[w];

        int cx = sx, cy = sy;
        int dx = up ? 0 : (down ? 0 : (left ? -1 : 1));
        int dy = up ? -1 : (down ? 1 : 0);

        do
        {
            isLoop[cy][cx] = true;
            cx += dx;
            cy += dy;
            char t = grid[cy][cx];
            if (t == 'L') { if (dy == 1) { dx = 1; dy = 0; } else { dx = 0; dy = -1; } }
            else if (t == 'J') { if (dy == 1) { dx = -1; dy = 0; } else { dx = 0; dy = -1; } }
            else if (t == '7') { if (dy == -1) { dx = -1; dy = 0; } else { dx = 0; dy = 1; } }
            else if (t == 'F') { if (dy == -1) { dx = 1; dy = 0; } else { dx = 0; dy = 1; } }
        } while (cx != sx || cy != sy);

        int enclosed = 0;
        for (int y = 0; y < h; y++)
        {
            bool inside = false;
            for (int x = 0; x < w; x++)
            {
                if (isLoop[y][x])
                {
                    char t = grid[y][x];
                    if (t == '|' || t == 'L' || t == 'J') inside = !inside;
                }
                else if (inside)
                {
                    enclosed++;
                }
            }
        }
        Console.WriteLine(enclosed);
    }
}
