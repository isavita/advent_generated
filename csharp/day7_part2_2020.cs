
using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

class BagRule
{
    public string Color { get; set; }
    public int Count { get; set; }
}

class Program
{
    static void Main()
    {
        Dictionary<string, List<BagRule>> rules = new Dictionary<string, List<BagRule>>();
        Regex ruleRegex = new Regex(@"(\d+) (\w+ \w+) bags?[,.]");

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                string[] parts = line.Split(new string[] { " bags contain " }, StringSplitOptions.None);
                string container = parts[0];
                string contents = parts[1];

                if (contents == "no other bags.")
                {
                    continue;
                }

                foreach (Match match in ruleRegex.Matches(contents))
                {
                    int count = int.Parse(match.Groups[1].Value);
                    string color = match.Groups[2].Value;
                    if (!rules.ContainsKey(container))
                    {
                        rules[container] = new List<BagRule>();
                    }
                    rules[container].Add(new BagRule { Color = color, Count = count });
                }
            }
        }

        int totalBags = CountBags("shiny gold", rules) - 1;
        Console.WriteLine(totalBags);
    }

    static int CountBags(string color, Dictionary<string, List<BagRule>> rules)
    {
        int count = 1;
        if (rules.ContainsKey(color))
        {
            foreach (BagRule rule in rules[color])
            {
                count += rule.Count * CountBags(rule.Color, rules);
            }
        }
        return count;
    }
}
