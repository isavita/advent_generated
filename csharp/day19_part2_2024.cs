
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.Error.WriteLine("Error: input.txt not found.");
            return;
        }

        var lines = File.ReadAllLines("input.txt");
        if (lines.Length < 2)
        {
            Console.Error.WriteLine("Error: Invalid input file format.");
            return;
        }

        var patterns = lines[0].Split(',')
            .Select(p => p.Trim())
            .Where(p => !string.IsNullOrEmpty(p))
            .ToArray();

        var patternLengths = patterns.Select(p => p.Length).ToArray();

        long totalWays = 0;
        for (int i = 2; i < lines.Length; i++)
        {
            var design = lines[i].Trim();
            if (!string.IsNullOrEmpty(design))
            {
                totalWays += CountWays(design, patterns, patternLengths);
            }
        }

        Console.WriteLine(totalWays);
    }

    public static long CountWays(string design, string[] patterns, int[] patternLengths)
    {
        var n = design.Length;
        var dp = new long[n + 1];
        dp[0] = 1;

        for (int i = 1; i <= n; i++)
        {
            for (int pIdx = 0; pIdx < patterns.Length; pIdx++)
            {
                var lp = patternLengths[pIdx];
                if (lp > 0 && i >= lp && design.Substring(i - lp, lp) == patterns[pIdx])
                {
                    dp[i] += dp[i - lp];
                }
            }
        }

        return dp[n];
    }
}
