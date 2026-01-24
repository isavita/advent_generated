
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    struct Coord { public int X, Y; }
    static char Empty = '.';

    static bool IsEmptyRow(char[,] grid, int y, int w)
    {
        for (int x = 0; x < w; x++) if (grid[y, x] != Empty) return false;
        return true;
    }

    static bool IsEmptyCol(char[,] grid, int x, int h)
    {
        for (int y = 0; y < h; y++) if (grid[y, x] != Empty) return false;
        return true;
    }

    static List<Coord> ExpandAndCollect(char[,] grid, int h, int w, int factor)
    {
        var list = new List<Coord>();
        int[] dy = new int[h], dx = new int[w];
        int emptyRows = 0, emptyCols = 0;

        for (int y = 0; y < h; y++)
        {
            dy[y] = emptyRows;
            if (IsEmptyRow(grid, y, w)) emptyRows += factor - 1;
        }
        for (int x = 0; x < w; x++)
        {
            dx[x] = emptyCols;
            if (IsEmptyCol(grid, x, h)) emptyCols += factor - 1;
        }

        for (int y = 0; y < h; y++)
            for (int x = 0; x < w; x++)
                if (grid[y, x] != Empty)
                    list.Add(new Coord { X = x + dx[x], Y = y + dy[y] });

        return list;
    }

    static long Solve(char[,] grid, int h, int w, int factor = 2)
    {
        var galaxies = ExpandAndCollect(grid, h, w, factor);
        long sum = 0;
        for (int i = 0; i < galaxies.Count; i++)
            for (int j = i + 1; j < galaxies.Count; j++)
                sum += Math.Abs(galaxies[j].X - galaxies[i].X) + Math.Abs(galaxies[j].Y - galaxies[i].Y);
        return sum;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int h = lines.Length, w = lines[0].Length;
        char[,] grid = new char[h, w];
        for (int y = 0; y < h; y++)
            for (int x = 0; x < w; x++)
                grid[y, x] = lines[y][x];

        Console.WriteLine(Solve(grid, h, w));
    }
}
