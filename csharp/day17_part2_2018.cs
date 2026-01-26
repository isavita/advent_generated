
using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static char[] Append(char[] a, char c)
    {
        var r = new char[a.Length + 1];
        Array.Copy(a, r, a.Length);
        r[a.Length] = c;
        return r;
    }
    static char[] Prepend(char[] a, char c)
    {
        var r = new char[a.Length + 1];
        Array.Copy(a, 0, r, 1, a.Length);
        r[0] = c;
        return r;
    }
    static char[][] Append(char[][] a, char[] row)
    {
        var r = new char[a.Length + 1][];
        Array.Copy(a, r, a.Length);
        r[a.Length] = row;
        return r;
    }

    static void Main()
    {
        var lines = new List<string>();
        foreach (var l in File.ReadLines("input.txt")) lines.Add(l.Trim());

        var ground = new char[1][];
        ground[0] = new[] { '+' };

        int maxX = 0, minX = 0, maxY = 0, minY = 20;
        const int xOffset = 500, yOffset = 0;
        var pat = new Regex(@"([xy])=(\d+), [xy]=(\\d+)..(\\d+)", RegexOptions.Compiled);
        pat = new Regex(@"([xy])=(\d+), [xy]=(\d+)..(\d+)", RegexOptions.Compiled);

        foreach (var l in lines)
        {
            var m = pat.Match(l);
            if (!m.Success) continue;
            var type = m.Groups[1].Value;
            int a = int.Parse(m.Groups[2].Value) - (type == "x" ? xOffset : yOffset);
            int b = int.Parse(m.Groups[3].Value) - (type == "x" ? yOffset : xOffset);
            int c = int.Parse(m.Groups[4].Value) - (type == "x" ? yOffset : xOffset);

            if (type == "x")
            {
                int x = a, y1 = b, y2 = c;
                while (x >= maxX) { maxX++; for (int i = 0; i < ground.Length; i++) ground[i] = Append(ground[i], '.'); }
                while (x <= minX) { minX--; for (int i = 0; i < ground.Length; i++) ground[i] = Prepend(ground[i], '.'); }
                while (y2 > maxY) { maxY++; ground = Append(ground, new char[ground[0].Length]); for (int j = 0; j < ground[ground.Length - 1].Length; j++) ground[ground.Length - 1][j] = '.'; }
                minY = Math.Min(minY, y1);
                for (int i = y1; i <= y2; i++) ground[i][x - minX] = '#';
            }
            else
            {
                int y = a, x1 = b, x2 = c;
                while (y > maxY) { maxY++; ground = Append(ground, new char[ground[0].Length]); for (int j = 0; j < ground[ground.Length - 1].Length; j++) ground[ground.Length - 1][j] = '.'; }
                while (x2 >= maxX) { maxX++; for (int i = 0; i < ground.Length; i++) ground[i] = Append(ground[i], '.'); }
                while (x1 <= minX) { minX--; for (int i = 0; i < ground.Length; i++) ground[i] = Prepend(ground[i], '.'); }
                for (int i = x1; i <= x2; i++) ground[y][i - minX] = '#';
                minY = Math.Min(minY, y);
            }
        }

        int water = 0, flow = 0, limit = 200000;
        while (ground[1][-minX] != '|' && water < limit)
        {
            bool moving = true;
            int x = -minX, y = 1, tryLeft = 0;
            while (moving)
            {
                if (y + 1 > maxY || ground[y + 1][x] == '|')
                {
                    ground[y][x] = '|';
                    moving = false;
                    if (y >= minY) flow++;
                }
                else if (ground[y + 1][x] == '.')
                {
                    y++;
                    tryLeft = 0;
                }
                else // below is # or ~
                {
                    if ((tryLeft == 1 && ground[y][x - 1] == '|') ||
                        (tryLeft == 2 && ground[y][x + 1] == '|') ||
                        (ground[y][x + 1] == '|' && ground[y][x - 1] != '.') ||
                        (ground[y][x + 1] != '.' && ground[y][x - 1] == '|'))
                    {
                        ground[y][x] = '|';
                        flow++;
                        moving = false;
                        for (int i = x + 1; i < ground[0].Length && ground[y][i] == '~'; i++) { ground[y][i] = '|'; water--; flow++; }
                        for (int i = x - 1; i >= 0 && ground[y][i] == '~'; i--) { ground[y][i] = '|'; water--; flow++; }
                    }
                    else if ((tryLeft == 0 && ground[y][x - 1] == '.') || (tryLeft == 1 && ground[y][x - 1] == '.'))
                    {
                        x--; tryLeft = 1;
                    }
                    else if ((tryLeft == 0 && ground[y][x + 1] == '.') || (tryLeft == 2 && ground[y][x + 1] == '.'))
                    {
                        x++; tryLeft = 2;
                    }
                    else
                    {
                        moving = false;
                        ground[y][x] = '~';
                        water++;
                    }
                }
            }
        }
        Console.WriteLine(water);
    }
}
