
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    struct Coord { public int X, Y; public Coord(int x, int y) { X = x; Y = y; } }
    static readonly Coord North = new Coord(0, -1);
    static readonly Coord West = new Coord(-1, 0);
    static readonly Coord South = new Coord(0, 1);
    static readonly Coord East = new Coord(1, 0);

    static void ShiftRocks(char[,] grid, int w, int h, Coord dir)
    {
        bool rev = dir.Y == 1 || dir.X == 1;
        int xFrom = rev ? w - 1 : 0, xTo = rev ? -1 : w, xStep = rev ? -1 : 1;
        int yFrom = rev ? h - 1 : 0, yTo = rev ? -1 : h, yStep = rev ? -1 : 1;
        for (int y = yFrom; y != yTo; y += yStep)
            for (int x = xFrom; x != xTo; x += xStep)
                if (grid[y, x] == 'O')
                {
                    int cx = x, cy = y;
                    while (true)
                    {
                        int nx = cx + dir.X, ny = cy + dir.Y;
                        if (nx < 0 || nx >= w || ny < 0 || ny >= h || grid[ny, nx] != '.') break;
                        grid[ny, nx] = 'O';
                        grid[cy, cx] = '.';
                        cx = nx; cy = ny;
                    }
                }
    }

    static int Load(char[,] grid, int w, int h)
    {
        int sum = 0;
        for (int y = 0; y < h; y++)
            for (int x = 0; x < w; x++)
                if (grid[y, x] == 'O') sum += h - y;
        return sum;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int h = lines.Length, w = lines[0].Length;
        var grid = new char[h, w];
        for (int y = 0; y < h; y++)
            for (int x = 0; x < w; x++)
                grid[y, x] = lines[y][x];

        ShiftRocks(grid, w, h, North);
        Console.WriteLine(Load(grid, w, h));
    }
}
