
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Rule
{
    public char Category;
    public char Operator;
    public int Num;
    public string WorkflowName;
    public Rule(char c, char op, int n, string w)
    {
        Category = c;
        Operator = op;
        Num = n;
        WorkflowName = w;
    }
}

struct Interval
{
    public int Start;
    public int End;
    public Interval(int s, int e) { Start = s; End = e; }
}

class Program
{
    static (Dictionary<string, List<Rule>>, List<Dictionary<char, int>>) ParseInput(string[] lines)
    {
        var workflows = new Dictionary<string, List<Rule>>();
        var parts = new List<Dictionary<char, int>>();

        int i = 0;
        while (i < lines.Length && !string.IsNullOrWhiteSpace(lines[i]))
        {
            var (name, rules) = ParseWorkflow(lines[i]);
            workflows[name] = rules;
            i++;
        }

        for (int j = i + 1; j < lines.Length; j++)
        {
            if (!string.IsNullOrWhiteSpace(lines[j]))
                parts.Add(ParsePart(lines[j]));
        }

        return (workflows, parts);
    }

    static (string, List<Rule>) ParseWorkflow(string line)
    {
        int braceIdx = line.IndexOf('{');
        string name = line.Substring(0, braceIdx);
        string body = line.Substring(braceIdx + 1, line.Length - braceIdx - 2); // remove braces
        var rules = new List<Rule>();

        foreach (var part in body.Split(','))
        {
            int colon = part.IndexOf(':');
            if (colon == -1)
            {
                // default rule, no condition
                rules.Add(new Rule(' ', ' ', 0, part));
            }
            else
            {
                char cat = part[0];
                char op = part[1];
                int num = int.Parse(part.Substring(2, colon - 2));
                string target = part.Substring(colon + 1);
                rules.Add(new Rule(cat, op, num, target));
            }
        }
        return (name, rules);
    }

    static Dictionary<char, int> ParsePart(string line)
    {
        // line looks like {x=787,m=2655,a=1222,s=2876}
        var vals = line.Trim('{', '}')
                       .Split(',')
                       .Select(p => int.Parse(p.Split('=')[1]))
                       .ToArray();
        return new Dictionary<char, int>
        {
            ['x'] = vals[0],
            ['m'] = vals[1],
            ['a'] = vals[2],
            ['s'] = vals[3]
        };
    }

    static long ApplyWorkflowInterval(
        Dictionary<char, Interval> part,
        Dictionary<string, List<Rule>> wf,
        string name)
    {
        if (name == "A")
        {
            long prod = 1;
            foreach (var iv in part.Values)
                prod *= (long)(iv.End - iv.Start + 1);
            return prod;
        }
        if (name == "R") return 0;

        long total = 0;
        foreach (var rule in wf[name])
        {
            // when rule has no condition it matches everything
            if (rule.Category == ' ')
            {
                total += ApplyWorkflowInterval(part, wf, rule.WorkflowName);
                continue;
            }

            var cur = part[rule.Category];
            Interval valid, invalid;

            if (rule.Operator == '>')
            {
                invalid = new Interval(cur.Start, Math.Min(cur.End, rule.Num));
                valid   = new Interval(Math.Max(cur.Start, rule.Num + 1), cur.End);
            }
            else // '<'
            {
                valid   = new Interval(cur.Start, Math.Min(cur.End, rule.Num - 1));
                invalid = new Interval(Math.Max(cur.Start, rule.Num), cur.End);
            }

            // branch where condition holds
            var next = new Dictionary<char, Interval>(part);
            next[rule.Category] = valid;
            total += ApplyWorkflowInterval(next, wf, rule.WorkflowName);

            // continue with the remaining interval for next rules
            part[rule.Category] = invalid;
            if (invalid.Start > invalid.End) break; // nothing left for further rules
        }

        return total;
    }

    static long Solve(string[] input)
    {
        const int min = 1;
        const int max = 4000;

        var (workflows, _) = ParseInput(input);

        var start = new Dictionary<char, Interval>
        {
            ['x'] = new Interval(min, max),
            ['m'] = new Interval(min, max),
            ['a'] = new Interval(min, max),
            ['s'] = new Interval(min, max)
        };

        return ApplyWorkflowInterval(start, workflows, "in");
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        Console.WriteLine(Solve(lines));
    }
}
