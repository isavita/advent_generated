
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class PointOfIncidence
{
    public static void Main(string[] args)
    {
        var patterns = ReadPatterns("input.txt");
        var sum = 0L;
        var sumWithSmudge = 0L;

        foreach (var pattern in patterns)
        {
            sum += SummarizePattern(pattern, -1);
            sumWithSmudge += SummarizePatternWithSmudge(pattern);
        }

        Console.WriteLine(sum);
        Console.WriteLine(sumWithSmudge);
    }

    private static List<string[]> ReadPatterns(string filename)
    {
        return File.ReadAllText(filename)
            .Split(new[] { "\r\n\r\n", "\n\n" }, StringSplitOptions.None)
            .Select(p => p.Split(new[] { "\r\n", "\n" }, StringSplitOptions.None))
            .ToList();
    }

    private static long SummarizePattern(string[] pattern, int ignore)
    {
        var rows = pattern.Length;
        var cols = pattern[0].Length;

        for (var c = 0; c < cols - 1; c++)
        {
            if (c + 1 == ignore) continue;
            var reflects = true;
            for (var i = 0; c - i >= 0 && c + 1 + i < cols; i++)
            {
                for (var r = 0; r < rows; r++)
                {
                    if (pattern[r][c - i] != pattern[r][c + 1 + i])
                    {
                        reflects = false;
                        break;
                    }
                }
                if (!reflects) break;
            }
            if (reflects) return c + 1;
        }

        for (var r = 0; r < rows - 1; r++)
        {
            if ((r + 1) * 100 == ignore) continue;
            var reflects = true;
            for (var i = 0; r - i >= 0 && r + 1 + i < rows; i++)
            {
                if (pattern[r - i] != pattern[r + 1 + i])
                {
                    reflects = false;
                    break;
                }
            }
            if (reflects) return (r + 1) * 100L;
        }

        return 0;
    }

    private static long SummarizePatternWithSmudge(string[] pattern)
    {
        var original = SummarizePattern(pattern, -1);
        var rows = pattern.Length;
        var cols = pattern[0].Length;

        for (var r = 0; r < rows; r++)
        {
            for (var c = 0; c < cols; c++)
            {
                var copy = (string[])pattern.Clone();
                copy[r] = copy[r].Substring(0, c) + (copy[r][c] == '.' ? '#' : '.') + copy[r].Substring(c + 1);
                var summary = SummarizePattern(copy, (int)original);
                if (summary > 0 && summary != original) return summary;
            }
        }
        return 0;
    }
}
