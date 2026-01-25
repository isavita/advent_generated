
using System;
using System.IO;

class Program
{
    static char[][] Allocate(int rows, int cols)
    {
        var g = new char[rows][];
        for (int i = 0; i < rows; i++)
        {
            g[i] = new char[cols];
            for (int j = 0; j < cols; j++) g[i][j] = '.';
        }
        return g;
    }

    static void Grow(ref char[][] g, ref int rows, ref int cols, int newRows, int newCols)
    {
        var ng = Allocate(newRows, newCols);
        for (int i = 0; i < rows; i++)
            Array.Copy(g[i], ng[i], cols);
        g = ng;
        rows = newRows;
        cols = newCols;
    }

    static void Shift(ref char[][] g, ref int cols, int shift)
    {
        int newCols = cols + shift;
        var ng = Allocate(g.Length, newCols);
        for (int i = 0; i < g.Length; i++)
        {
            for (int j = 0; j < cols; j++) ng[i][j + shift] = g[i][j];
        }
        g = ng;
        cols = newCols;
    }

    static int ToInt(string s)
    {
        int v = 0, sign = 1, i = 0;
        if (s[0] == '-') { sign = -1; i = 1; }
        for (; i < s.Length; i++) v = v * 10 + (s[i] - '0');
        return v * sign;
    }

    static void Parse(string line, ref char[][] g, ref int rows, ref int cols,
                      ref int minX, ref int maxX, ref int minY, ref int maxY,
                      int xOff, int yOff)
    {
        var parts = line.Replace('=', ' ')
                        .Replace(',', ' ')
                        .Replace('.', ' ')
                        .Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
        if (parts[0] == "x")
        {
            int x = ToInt(parts[1]) - xOff;
            int y1 = ToInt(parts[3]) - yOff;
            int y2 = ToInt(parts[4]) - yOff;

            while (x >= maxX) { maxX++; Grow(ref g, ref rows, ref cols, rows, maxX - minX + 1); }
            while (x <= minX) { minX--; Shift(ref g, ref cols, 1); }
            while (y2 > maxY) { maxY++; Grow(ref g, ref rows, ref cols, maxY + 1, cols); }
            if (y1 < minY) minY = y1;
            for (int y = y1; y <= y2; y++) g[y][x - minX] = '#';
        }
        else
        {
            int y = ToInt(parts[1]) - yOff;
            int x1 = ToInt(parts[3]) - xOff;
            int x2 = ToInt(parts[4]) - xOff;

            while (y > maxY) { maxY++; Grow(ref g, ref rows, ref cols, maxY + 1, cols); }
            while (x2 >= maxX) { maxX++; Grow(ref g, ref rows, ref cols, rows, maxX - minX + 1); }
            while (x1 <= minX) { minX--; Shift(ref g, ref cols, 1); }
            for (int x = x1; x <= x2; x++) g[y][x - minX] = '#';
            if (y < minY) minY = y;
        }
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int rows = 1, cols = 1, minX = 0, maxX = 0, minY = 20, maxY = 0;
        int xOff = 500, yOff = 0;
        var ground = Allocate(rows, cols);
        ground[0][0] = '+';

        foreach (var l in lines) Parse(l, ref ground, ref rows, ref cols,
                                      ref minX, ref maxX, ref minY, ref maxY,
                                      xOff, yOff);

        int water = 0, flow = 0, limit = 200000;
        while (ground[1][-minX] != '|' && water < limit)
        {
            int x = -minX, y = 1, tryLeft = 0;
            bool can = true;
            while (can)
            {
                if (y + 1 > maxY || ground[y + 1][x] == '|')
                {
                    ground[y][x] = '|'; can = false;
                    if (y >= minY) flow++;
                }
                else if (ground[y + 1][x] == '.')
                {
                    y++; tryLeft = 0;
                }
                else // below is # or ~
                {
                    bool leftFlow = (tryLeft == 1 && ground[y][x - 1] == '|');
                    bool rightFlow = (tryLeft == 2 && ground[y][x + 1] == '|');
                    bool cond1 = (ground[y][x + 1] == '|' && ground[y][x - 1] != '.');
                    bool cond2 = (ground[y][x + 1] != '.' && ground[y][x - 1] == '|');
                    if (leftFlow || rightFlow || cond1 || cond2)
                    {
                        ground[y][x] = '|'; flow++;
                        can = false;
                        for (int i = x + 1; ground[y][i] == '~'; i++) { ground[y][i] = '|'; water--; flow++; }
                        for (int i = x - 1; ground[y][i] == '~'; i--) { ground[y][i] = '|'; water--; flow++; }
                    }
                    else if ((tryLeft == 0 && ground[y][x - 1] == '.') ||
                             (tryLeft == 1 && ground[y][x - 1] == '.'))
                    {
                        x--; tryLeft = 1;
                    }
                    else if ((tryLeft == 0 && ground[y][x + 1] == '.') ||
                             (tryLeft == 2 && ground[y][x + 1] == '.'))
                    {
                        x++; tryLeft = 2;
                    }
                    else
                    {
                        can = false;
                        ground[y][x] = '~'; water++;
                    }
                }
            }
        }
        Console.WriteLine(flow + water);
    }
}
