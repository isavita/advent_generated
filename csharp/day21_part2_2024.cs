
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Solution
{
    private class Key
    {
        public string Code { get; }
        public int Robots { get; }
        public int MaxRobots { get; }

        public Key(string code, int robots, int maxRobots)
        {
            Code = code;
            Robots = robots;
            MaxRobots = maxRobots;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || GetType() != obj.GetType())
                return false;

            var key = (Key)obj;
            return Robots == key.Robots && MaxRobots == key.MaxRobots && Code == key.Code;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hash = 17;
                hash = hash * 23 + Code.GetHashCode();
                hash = hash * 23 + Robots.GetHashCode();
                hash = hash * 23 + MaxRobots.GetHashCode();
                return hash;
            }
        }
    }

    private static int[] FindPosition(string[] mat, char ch)
    {
        for (int i = 0; i < mat.Length; i++)
        {
            int j = mat[i].IndexOf(ch);
            if (j != -1)
                return new int[] { i, j };
        }
        return new int[] { -1, -1 };
    }

    private static bool Ok(string[] mat, int[] st, string seq)
    {
        int currI = st[0];
        int currJ = st[1];
        foreach (char ch in seq)
        {
            if (!(0 <= currI && currI < mat.Length && 0 <= currJ && currJ < mat[currI].Length) || mat[currI][currJ] == ' ')
                return false;

            switch (ch)
            {
                case '^':
                    currI -= 1;
                    break;
                case 'v':
                    currI += 1;
                    break;
                case '<':
                    currJ -= 1;
                    break;
                case '>':
                    currJ += 1;
                    break;
            }
        }
        return true;
    }

    private static string GenerateMoves(int[] position, char objective, string[] pad)
    {
        int[] objPos = FindPosition(pad, objective);
        int posI = position[0];
        int posJ = position[1];
        int objPosI = objPos[0];
        int objPosJ = objPos[1];

        string result = "";
        if (posJ > objPosJ)
            result += new string('<', posJ - objPosJ);
        if (posI > objPosI)
            result += new string('^', posI - objPosI);
        if (posI < objPosI)
            result += new string('v', objPosI - posI);
        if (posJ < objPosJ)
            result += new string('>', objPosJ - posJ);

        if (!Ok(pad, position, result))
        {
            result = "";
            if (posJ < objPosJ)
                result += new string('>', objPosJ - posJ);
            if (posI > objPosI)
                result += new string('^', posI - objPosI);
            if (posI < objPosI)
                result += new string('v', objPosI - posI);
            if (posJ > objPosJ)
                result += new string('<', posJ - objPosJ);
        }

        return result;
    }

    private static long Solve(string code, int robots, string[] keyPad, string[] robotPad, int maxRobots, Dictionary<Key, long> memo)
    {
        var key = new Key(code, robots, maxRobots);
        if (memo.TryGetValue(key, out long value))
            return value;

        if (robots <= 0)
            return code.Length;

        long ret = 0;
        int posI = 3, posJ = 2;
        if (robots != maxRobots)
            posI = 0;

        foreach (char ch in code)
        {
            string moves;
            if (robots == maxRobots)
            {
                moves = GenerateMoves(new int[] { posI, posJ }, ch, keyPad);
                int[] pos = FindPosition(keyPad, ch);
                posI = pos[0];
                posJ = pos[1];
            }
            else
            {
                moves = GenerateMoves(new int[] { posI, posJ }, ch, robotPad);
                int[] pos = FindPosition(robotPad, ch);
                posI = pos[0];
                posJ = pos[1];
            }
            ret += Solve(moves + "A", robots - 1, keyPad, robotPad, maxRobots, memo);
        }

        memo[key] = ret;
        return ret;
    }

    public static void Main(string[] args)
    {
        string[] content = File.ReadAllLines("input.txt");

        int maxRobots = 26;
        string[] keyPad = new string[] { "789", "456", "123", " 0A" };
        string[] robotPad = new string[] { " ^A", "<v>" };

        long ret = 0;
        var memo = new Dictionary<Key, long>();
        foreach (string line in content)
        {
            string code = line.Trim();
            if (string.IsNullOrEmpty(code))
                continue;

            long numericPart = 0;
            foreach (char c in code)
            {
                if (char.IsDigit(c))
                    numericPart = numericPart * 10 + (c - '0');
            }

            ret += Solve(code, maxRobots, keyPad, robotPad, maxRobots, memo) * numericPart;
            memo.Clear(); // Clear memoization dictionary for each new code
        }

        Console.WriteLine(ret);
    }
}
