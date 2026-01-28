
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var text = File.ReadAllText("input.txt");
        var parts = text.Split(new[] { "\n\n", "\r\n\r\n" }, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length < 2) return;
        Console.WriteLine(Solve(parts[0], parts[1], false));
        Console.WriteLine(Solve(parts[0], parts[1], true));
    }

    static long Solve(string gStr, string mStr, bool p2)
    {
        if (p2) gStr = gStr.Replace("#", "##").Replace(".", "..").Replace("O", "[]").Replace("@", "@.");
        var lines = gStr.Split(new[] { '\n', '\r' }, StringSplitOptions.RemoveEmptyEntries);
        var g = lines.Select(l => l.ToCharArray()).ToArray();
        int r = -1, c = -1, rows = g.Length, cols = g[0].Length;
        for (int i = 0; i < rows; i++)
            for (int j = 0; j < cols; j++)
                if (g[i][j] == '@') { r = i; c = j; }

        foreach (var m in mStr)
        {
            int dr = m == '^' ? -1 : m == 'v' ? 1 : 0;
            int dc = m == '<' ? -1 : m == '>' ? 1 : 0;
            if (dr == 0 && dc == 0) continue;
            if (CanMove(g, r, c, dr, dc))
            {
                Move(g, r, c, dr, dc);
                r += dr; c += dc;
            }
        }
        long sum = 0;
        for (int i = 0; i < rows; i++)
            for (int j = 0; j < cols; j++)
                if (g[i][j] == 'O' || g[i][j] == '[') sum += 100 * i + j;
        return sum;
    }

    static bool CanMove(char[][] g, int r, int c, int dr, int dc)
    {
        char curr = g[r][c];
        if (curr == '.') return true;
        if (curr == '#') return false;
        if (dr != 0)
        {
            if (curr == '[') return CanMove(g, r + dr, c, dr, dc) && CanMove(g, r + dr, c + 1, dr, dc);
            if (curr == ']') return CanMove(g, r + dr, c, dr, dc) && CanMove(g, r + dr, c - 1, dr, dc);
        }
        return CanMove(g, r + dr, c + dc, dr, dc);
    }

    static void Move(char[][] g, int r, int c, int dr, int dc)
    {
        if (g[r][c] == '.') return;
        if (dr != 0 && (g[r][c] == '[' || g[r][c] == ']'))
        {
            int cL = g[r][c] == '[' ? c : c - 1;
            if (g[r][cL] == '.') return;
            Move(g, r + dr, cL, dr, dc);
            Move(g, r + dr, cL + 1, dr, dc);
            g[r + dr][cL] = '[';
            g[r + dr][cL + 1] = ']';
            g[r][cL] = '.';
            g[r][cL + 1] = '.';
        }
        else
        {
            Move(g, r + dr, c + dc, dr, dc);
            g[r + dr][c + dc] = g[r][c];
            g[r][c] = '.';
        }
    }
}

