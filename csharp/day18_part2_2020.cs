using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Program
{
    static long EvaluateSimple(string expr)
    {
        var parts = new List<string>();
        foreach (Match m in Regex.Matches(expr, @"\d+|[+*]")) parts.Add(m.Value);
        long total = long.Parse(parts[0]);
        for (int i = 1; i < parts.Count; i += 2)
            total = parts[i] == "+" ? total + long.Parse(parts[i + 1]) : total * long.Parse(parts[i + 1]);
        return total;
    }

    static long EvaluateAdvanced(string expr)
    {
        var parts = new List<string>();
        foreach (Match m in Regex.Matches(expr, @"\d+|[+*]")) parts.Add(m.Value);
        int idx;
        while ((idx = parts.IndexOf("+")) != -1)
        {
            long sum = long.Parse(parts[idx - 1]) + long.Parse(parts[idx + 1]);
            parts.RemoveRange(idx - 1, 3);
            parts.Insert(idx - 1, sum.ToString());
        }
        long total = long.Parse(parts[0]);
        for (int i = 1; i < parts.Count; i += 2) total *= long.Parse(parts[i + 1]);
        return total;
    }

    static long Evaluate(string expr, bool adv)
    {
        while (expr.Contains('('))
        {
            int start = expr.LastIndexOf('(');
            int end = start;
            int depth = 0;
            for (int i = start; i < expr.Length; i++)
            {
                if (expr[i] == '(') depth++;
                else if (expr[i] == ')')
                {
                    depth--;
                    if (depth == 0) { end = i; break; }
                }
            }
            string inner = expr.Substring(start + 1, end - start - 1);
            long val = adv ? EvaluateAdvanced(inner) : EvaluateSimple(inner);
            expr = expr.Substring(0, start) + val.ToString() + expr.Substring(end + 1);
        }
        return adv ? EvaluateAdvanced(expr) : EvaluateSimple(expr);
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        long part1 = 0, part2 = 0;
        foreach (var line in lines)
        {
            part1 += Evaluate(line, false);
            part2 += Evaluate(line, true);
        }
        Console.WriteLine(part1);
        Console.WriteLine(part2);
    }
}