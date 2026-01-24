
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static List<char[]> grid = new List<char[]>();
    static string moves = "";
    static int robotR = 0, robotC = 0;

    static bool PushBoxes(int r, int c, int dr, int dc)
    {
        int nr = r + dr;
        int nc = c + dc;
        if (nr < 0 || nr >= grid.Count || nc < 0 || nc >= grid[0].Length) return false;
        if (grid[nr][nc] == '#') return false;
        if (grid[nr][nc] == 'O' && !PushBoxes(nr, nc, dr, dc)) return false;
        grid[nr][nc] = 'O';
        grid[r][c] = '.';
        return true;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        bool readingMap = true;
        foreach (var line in lines)
        {
            if (string.IsNullOrEmpty(line))
            {
                if (grid.Count > 0) readingMap = false;
                continue;
            }
            if (readingMap && line.IndexOfAny(new[] { '#', '@', 'O', '.' }) >= 0)
            {
                grid.Add(line.ToCharArray());
                int idx = line.IndexOf('@');
                if (idx >= 0) { robotR = grid.Count - 1; robotC = idx; }
            }
            else
            {
                moves += line;
            }
        }

        foreach (char move in moves)
        {
            int dr = 0, dc = 0;
            switch (move)
            {
                case '^': dr = -1; break;
                case 'v': dr = 1; break;
                case '<': dc = -1; break;
                case '>': dc = 1; break;
                default: continue;
            }
            int nr = robotR + dr;
            int nc = robotC + dc;
            if (nr < 0 || nr >= grid.Count || nc < 0 || nc >= grid[0].Length) continue;
            char next = grid[nr][nc];
            if (next == '#') continue;
            if (next == 'O' && !PushBoxes(nr, nc, dr, dc)) continue;
            grid[robotR][robotC] = '.';
            grid[nr][nc] = '@';
            robotR = nr;
            robotC = nc;
        }

        long total = 0;
        for (int r = 0; r < grid.Count; r++)
            for (int c = 0; c < grid[0].Length; c++)
                if (grid[r][c] == 'O')
                    total += r * 100L + c;
        Console.WriteLine(total);
    }
}
