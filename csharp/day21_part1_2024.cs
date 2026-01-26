
using System;
using System.IO;
using System.Collections.Generic;
using System.Text;

class Program
{
    static int[] FindPos(string[] m, char c)
    {
        for (int i = 0; i < m.Length; i++)
            for (int j = 0; j < m[i].Length; j++)
                if (m[i][j] == c) return new[] { i, j };
        return new[] { -1, -1 };
    }

    static bool Ok(string[] m, int[] s, string seq)
    {
        int i = s[0], j = s[1];
        foreach (char ch in seq)
        {
            if (m[i][j] == ' ') return false;
            i += ch == 'v' ? 1 : ch == '^' ? -1 : 0;
            j += ch == '>' ? 1 : ch == '<' ? -1 : 0;
            if (i < 0 || i >= m.Length || j < 0 || j >= m[0].Length) return false;
        }
        return true;
    }

    static string GenMoves(int[] pos, char obj, string[] pad)
    {
        var objPos = FindPos(pad, obj);
        int oi = objPos[0], oj = objPos[1];
        int pi = pos[0], pj = pos[1];
        var sb = new StringBuilder();
        if (pj > oj) sb.Append(new string('<', pj - oj));
        if (pi > oi) sb.Append(new string('^', pi - oi));
        if (pi < oi) sb.Append(new string('v', oi - pi));
        if (pj < oj) sb.Append(new string('>', oj - pj));
        var cand = sb.ToString();
        if (Ok(pad, pos, cand)) return cand;
        sb.Clear();
        if (pj < oj) sb.Append(new string('>', oj - pj));
        if (pi > oi) sb.Append(new string('^', pi - oi));
        if (pi < oi) sb.Append(new string('v', oi - pi));
        if (pj > oj) sb.Append(new string('<', pj - oj));
        return sb.ToString();
    }

    static long Solve(string code, int robots, string[] keyPad, string[] robotPad, int maxRobots, Dictionary<string, long> memo)
    {
        if (robots <= 0) return code.Length;
        var state = code + "," + robots;
        if (memo.TryGetValue(state, out var cached)) return cached;
        long sum = 0;
        int pi = robots == maxRobots ? 3 : 0, pj = robots == maxRobots ? 2 : 2;
        foreach (char ch in code)
        {
            string moves;
            int[] np;
            if (robots == maxRobots)
            {
                moves = GenMoves(new[] { pi, pj }, ch, keyPad);
                np = FindPos(keyPad, ch);
            }
            else
            {
                moves = GenMoves(new[] { pi, pj }, ch, robotPad);
                np = FindPos(robotPad, ch);
            }
            pi = np[0];
            pj = np[1];
            sum += Solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots, memo);
        }
        memo[state] = sum;
        return sum;
    }

    static void Main()
    {
        const int maxRobots = 3;
        string[] keyPad = { "789", "456", "123", " 0A" };
        string[] robotPad = { " ^A", "<v>" };
        long total = 0;
        foreach (var line in File.ReadLines("input.txt"))
        {
            var s = line.Trim();
            if (s.Length == 0) continue;
            int num = 0;
            foreach (char c in s) if (char.IsDigit(c)) num = num * 10 + (c - '0');
            var memo = new Dictionary<string, long>();
            total += Solve(s, maxRobots, keyPad, robotPad, maxRobots, memo) * num;
        }
        Console.WriteLine(total);
    }
}
