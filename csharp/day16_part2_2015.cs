
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        int res = AuntSue(input);
        Console.WriteLine(res);
    }

    static Dictionary<string, int> targetSue = new Dictionary<string, int>
    {
        { "children", 3 },
        { "cats", 7 },
        { "samoyeds", 2 },
        { "pomeranians", 3 },
        { "akitas", 0 },
        { "vizslas", 0 },
        { "goldfish", 5 },
        { "trees", 3 },
        { "cars", 2 },
        { "perfumes", 1 }
    };

    static int AuntSue(string input)
    {
        foreach (string line in input.Split('\n'))
        {
            string[] parts = line.Split(' ');
            int sueNum = int.Parse(parts[1].Trim(':'));
            string thing1 = parts[2].Trim(':');
            int amount1 = int.Parse(parts[3].Trim(','));
            string thing2 = parts[4].Trim(':');
            int amount2 = int.Parse(parts[5].Trim(','));
            string thing3 = parts[6].Trim(':');
            int amount3 = int.Parse(parts[7]);

            Dictionary<string, int> readingsMap = new Dictionary<string, int>
            {
                { thing1, amount1 },
                { thing2, amount2 },
                { thing3, amount3 }
            };

            bool allRulesMatched = true;

            foreach (var check in new string[] { "cats", "trees" })
            {
                if (readingsMap.ContainsKey(check))
                {
                    if (readingsMap[check] <= targetSue[check])
                    {
                        allRulesMatched = false;
                    }
                    readingsMap.Remove(check);
                }
            }

            foreach (var check in new string[] { "pomeranians", "goldfish" })
            {
                if (readingsMap.ContainsKey(check))
                {
                    if (readingsMap[check] >= targetSue[check])
                    {
                        allRulesMatched = false;
                    }
                    readingsMap.Remove(check);
                }
            }

            foreach (var kvp in readingsMap)
            {
                if (targetSue[kvp.Key] != kvp.Value)
                {
                    allRulesMatched = false;
                }
            }

            if (allRulesMatched)
            {
                return sueNum;
            }
        }

        throw new Exception("Expect return from loop");
    }
}
