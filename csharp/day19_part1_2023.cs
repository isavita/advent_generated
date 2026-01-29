using System;
using System.IO;
using System.Collections.Generic;

class Rule
{
    public char Category;
    public char Op;
    public int Value;
    public short DestIdx;
    public Rule()
    {
        Category = '\0';
        Op = '\0';
        Value = 0;
        DestIdx = -1;
    }
}

class Workflow
{
    public string Name;
    public Rule[] Rules;
    public int NumRules;
    public Workflow(string name)
    {
        Name = name;
        Rules = new Rule[10];
        for (int i = 0; i < 10; i++) Rules[i] = new Rule();
        NumRules = 0;
    }
}

struct Part
{
    public int x, m, a, s;
}

class Program
{
    static List<Workflow> workflows = new List<Workflow>();
    static List<Part> parts = new List<Part>();

    static void Main(string[] args)
    {
        ParseInput("input.txt");
        int inIdx = GetWorkflowIndex("in");
        long total = 0;
        foreach (var p in parts)
        {
            if (EvaluatePart(p, inIdx))
                total += p.x + p.m + p.a + p.s;
        }
        Console.WriteLine(total);
    }

    static int GetWorkflowIndex(string name)
    {
        if (name == "A" && name.Length == 1) return -1;
        if (name == "R" && name.Length == 1) return -2;
        for (int i = 0; i < workflows.Count; i++)
            if (workflows[i].Name == name) return i;
        workflows.Add(new Workflow(name));
        return workflows.Count - 1;
    }

    static void ParseInput(string filename)
    {
        using (var sr = new StreamReader(filename))
        {
            bool parsingWorkflows = true;
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                line = line.Trim();
                if (line.Length == 0)
                {
                    parsingWorkflows = false;
                    continue;
                }

                if (parsingWorkflows)
                {
                    int braceIndex = line.IndexOf('{');
                    if (braceIndex < 0) continue;

                    string name = line.Substring(0, braceIndex);
                    int wfIdx = GetWorkflowIndex(name);
                    workflows[wfIdx].NumRules = 0;

                    string rulePart = line.Substring(braceIndex + 1).TrimEnd('}');
                    string[] ruleStrings = rulePart.Split(new char[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
                    foreach (var r in ruleStrings)
                    {
                        string token = r.Trim();
                        if (token.Length > 0)
                            ParseRule(workflows[wfIdx], token);
                    }
                }
                else
                {
                    if (parts.Count < 600)
                    {
                        Part p = new Part();
                        string inner = line;
                        int b = inner.IndexOf('{');
                        if (b >= 0) inner = inner.Substring(b + 1);
                        inner = inner.Replace("}", "");
                        string[] kvs = inner.Split(',');
                        foreach (var kv in kvs)
                        {
                            var pv = kv.Split('=');
                            if (pv.Length != 2) continue;
                            string key = pv[0].Trim();
                            string val = pv[1].Trim();
                            switch (key)
                            {
                                case "x": p.x = int.Parse(val); break;
                                case "m": p.m = int.Parse(val); break;
                                case "a": p.a = int.Parse(val); break;
                                case "s": p.s = int.Parse(val); break;
                            }
                        }
                        parts.Add(p);
                    }
                }
            }
        }
    }

    static void ParseRule(Workflow wf, string ruleStr)
    {
        Rule rule = new Rule();
        int colonIndex = ruleStr.IndexOf(':');
        string destName;
        if (colonIndex >= 0)
        {
            rule.Category = ruleStr[0];
            rule.Op = ruleStr[1];
            rule.Value = int.Parse(ruleStr.Substring(2, colonIndex - 2));
            destName = ruleStr.Substring(colonIndex + 1);
        }
        else
        {
            rule.Category = '\0';
            rule.Op = '\0';
            rule.Value = 0;
            destName = ruleStr;
        }
        rule.DestIdx = (short)GetWorkflowIndex(destName);
        if (wf.NumRules < wf.Rules.Length)
            wf.Rules[wf.NumRules++] = rule;
    }

    static bool EvaluatePart(Part part, int startIdx)
    {
        int currentIdx = startIdx;
        while (currentIdx >= 0)
        {
            Workflow wf = workflows[currentIdx];
            bool ruleMatched = false;
            for (int i = 0; i < wf.NumRules; i++)
            {
                Rule rule = wf.Rules[i];
                bool conditionMet;
                if (rule.Category == '\0')
                {
                    conditionMet = true;
                }
                else
                {
                    int partValue;
                    switch (rule.Category)
                    {
                        case 'x': partValue = part.x; break;
                        case 'm': partValue = part.m; break;
                        case 'a': partValue = part.a; break;
                        case 's': partValue = part.s; break;
                        default: partValue = 0; break;
                    }
                    if (rule.Op == '<') conditionMet = partValue < rule.Value;
                    else if (rule.Op == '>') conditionMet = partValue > rule.Value;
                    else conditionMet = false;
                }

                if (conditionMet)
                {
                    currentIdx = rule.DestIdx;
                    ruleMatched = true;
                    break;
                }
            }
            if (!ruleMatched) return false;
            if (currentIdx == -1) return true;
            if (currentIdx == -2) return false;
        }
        return false;
    }

    static void GreedyInit()
    {
        // placeholder to emphasize no extra initialization required
    }
}