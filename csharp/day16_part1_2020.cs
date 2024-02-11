
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Linq;

class Rule
{
    public string name;
    public int[][] ranges;

    public bool IsValid(int value)
    {
        foreach (var rng in ranges)
        {
            if (value >= rng[0] && value <= rng[1])
            {
                return true;
            }
        }
        return false;
    }
}

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Rule[] rules = new Rule[0];
        bool scanningRules = true;
        int errorRate = 0;

        Regex reRule = new Regex(@"^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$");

        foreach (string line in lines)
        {
            if (line == "")
            {
                continue;
            }
            if (line.StartsWith("your ticket:") || line.StartsWith("nearby tickets:"))
            {
                scanningRules = false;
                continue;
            }
            if (scanningRules)
            {
                Match match = reRule.Match(line);
                if (match.Success)
                {
                    string name = match.Groups[1].Value;
                    int range1Start = int.Parse(match.Groups[2].Value);
                    int range1End = int.Parse(match.Groups[3].Value);
                    int range2Start = int.Parse(match.Groups[4].Value);
                    int range2End = int.Parse(match.Groups[5].Value);
                    Rule rule = new Rule
                    {
                        name = name,
                        ranges = new int[][] { new int[] { range1Start, range1End }, new int[] { range2Start, range2End } }
                    };
                    rules = rules.Append(rule).ToArray();
                }
            }
            else
            {
                foreach (string value in line.Split(','))
                {
                    int val = int.Parse(value);
                    if (!IsValidForAnyRule(val, rules))
                    {
                        errorRate += val;
                    }
                }
            }
        }

        Console.WriteLine(errorRate);
    }

    static bool IsValidForAnyRule(int value, Rule[] rules)
    {
        foreach (var rule in rules)
        {
            if (rule.IsValid(value))
            {
                return true;
            }
        }
        return false;
    }
}
