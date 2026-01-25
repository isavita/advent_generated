
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public class Program
{
    private static Dictionary<int, Rule> rules = new Dictionary<int, Rule>();
    private static List<string> messages = new List<string>();

    public static void Main(string[] args)
    {
        var input = File.ReadAllText("input.txt");
        ParseInput(input);
        var result = Solve();
        Console.WriteLine(result);
    }

    private static void ParseInput(string input)
    {
        var parts = input.Split(new[] { "\r\n\r\n", "\n\n" }, StringSplitOptions.None);
        var rulePart = parts[0];
        var messagePart = parts[1];

        foreach (var line in rulePart.Split(new[] { "\r\n", "\n" }, StringSplitOptions.None))
        {
            var match = Regex.Match(line, @"(\d+): (.*)");
            if (match.Success)
            {
                var ruleNum = int.Parse(match.Groups[1].Value);
                var ruleDef = match.Groups[2].Value;
                rules[ruleNum] = new Rule(ruleDef);
            }
        }

        messages.AddRange(messagePart.Split(new[] { "\r\n", "\n" }, StringSplitOptions.None).Where(m => !string.IsNullOrEmpty(m)));
    }

    private static int Solve()
    {
        foreach (var rule in rules.Values)
        {
            rule.Resolve(rules);
        }

        var rule42 = rules[42].ResolvedStrings;
        var rule31 = rules[31].ResolvedStrings;

        var chunkLen = rule42[0].Length;
        var matchCount = 0;

        foreach (var msg in messages)
        {
            if (msg.Length % chunkLen != 0) continue;

            var numChunks = msg.Length / chunkLen;
            if (numChunks < 3) continue;

            var count42 = 0;
            var count31 = 0;

            for (var k = 0; k < numChunks; k++)
            {
                var chunk = msg.Substring(k * chunkLen, chunkLen);
                if (rule42.Contains(chunk))
                {
                    count42++;
                }
                else
                {
                    break;
                }
            }

            for (var k = count42; k < numChunks; k++)
            {
                var chunk = msg.Substring(k * chunkLen, chunkLen);
                if (rule31.Contains(chunk))
                {
                    count31++;
                }
                else
                {
                    break;
                }
            }

            if (count42 + count31 == numChunks && count31 >= 1 && count42 > count31)
            {
                matchCount++;
            }
        }

        return matchCount;
    }
}

public class Rule
{
    public List<string> ResolvedStrings { get; private set; } = new List<string>();
    public List<Option> Options { get; private set; } = new List<Option>();

    public Rule(string definition)
    {
        if (definition.StartsWith("\"") && definition.EndsWith("\""))
        {
            ResolvedStrings.Add(definition.Substring(1, definition.Length - 2));
        }
        else
        {
            var orParts = definition.Split('|');
            foreach (var orPart in orParts)
            {
                var andParts = orPart.Trim().Split(' ');
                var option = new Option();
                foreach (var andPart in andParts)
                {
                    option.SubRules.Add(int.Parse(andPart));
                }
                Options.Add(option);
            }
        }
    }

    public void Resolve(Dictionary<int, Rule> rules)
    {
        if (ResolvedStrings.Count > 0) return;

        foreach (var option in Options)
        {
            var currentResolved = new List<string> { "" };
            foreach (var subRule in option.SubRules)
            {
                var subRuleResolved = rules[subRule];
                subRuleResolved.Resolve(rules);

                var nextResolved = new List<string>();
                foreach (var current in currentResolved)
                {
                    foreach (var subRuleString in subRuleResolved.ResolvedStrings)
                    {
                        nextResolved.Add(current + subRuleString);
                    }
                }
                currentResolved = nextResolved;
            }

            ResolvedStrings.AddRange(currentResolved);
        }
    }
}

public class Option
{
    public List<int> SubRules { get; private set; } = new List<int>();
}
