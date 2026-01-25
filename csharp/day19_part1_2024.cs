using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static bool CanMake(string design, List<string> patterns)
    {
        int n = design.Length;
        bool[] dp = new bool[n + 1];
        dp[0] = true;
        for (int i = 1; i <= n; i++)
        {
            foreach (var p in patterns)
            {
                int len = p.Length;
                if (len == 0) continue;
                if (i >= len && dp[i - len] && design.Substring(i - len, len) == p)
                {
                    dp[i] = true;
                    break;
                }
            }
        }
        return dp[n];
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        if (lines.Length == 0) return;
        var patterns = new List<string>();
        foreach (var token in lines[0].Split(','))
            patterns.Add(token.Trim());
        int count = 0;
        for (int idx = 2; idx < lines.Length; idx++)
        {
            string design = lines[idx].Trim();
            if (design.Length == 0) continue;
            if (CanMake(design, patterns)) count++;
        }
        Console.WriteLine(count);
    }
}
