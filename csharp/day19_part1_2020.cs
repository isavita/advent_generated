
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Solution
{
    static Dictionary<int, string> ReadRules(StreamReader reader)
    {
        var rules = new Dictionary<int, string>();
        string line;
        while ((line = reader.ReadLine()) != null)
        {
            if (line == "")
            {
                break;
            }
            string[] parts = line.Split(": ");
            rules[int.Parse(parts[0])] = parts[1].Replace("\"", "");
        }
        return rules;
    }

    static string ConstructPattern(Dictionary<int, string> rules, int index)
    {
        if (rules[index].Contains("|"))
        {
            string[] subrules = rules[index].Split(" | ");
            List<string> parts = new List<string>();
            foreach (var subrule in subrules)
            {
                parts.Add(ConstructSubPattern(rules, subrule));
            }
            return "(" + string.Join("|", parts) + ")";
        }
        return ConstructSubPattern(rules, rules[index]);
    }

    static string ConstructSubPattern(Dictionary<int, string> rules, string subrule)
    {
        if (subrule == "a" || subrule == "b")
        {
            return subrule;
        }
        string[] subIdxs = subrule.Split(" ");
        string pattern = "";
        foreach (var idx in subIdxs)
        {
            pattern += ConstructPattern(rules, int.Parse(idx));
        }
        return pattern;
    }

    static int CountMatches(StreamReader reader, string pattern)
    {
        int count = 0;
        Regex re = new Regex("^" + pattern + "$");
        string message;
        while ((message = reader.ReadLine()) != null)
        {
            if (re.IsMatch(message))
            {
                count++;
            }
        }
        return count;
    }

    static void Main()
    {
        using (StreamReader reader = new StreamReader("input.txt"))
        {
            Dictionary<int, string> rules = ReadRules(reader);
            string pattern = ConstructPattern(rules, 0);
            int count = CountMatches(reader, pattern);

            Console.WriteLine("The number of messages that completely match rule 0 is: " + count);
        }
    }
}
