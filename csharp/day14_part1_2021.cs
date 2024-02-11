
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        string polymer = lines[0];
        Dictionary<string, string> rules = new Dictionary<string, string>();

        for (int i = 1; i < lines.Length; i++)
        {
            string line = lines[i];
            if (line == "")
            {
                continue;
            }
            string[] parts = line.Split(new string[] { " -> " }, StringSplitOptions.None);
            rules[parts[0]] = parts[1];
        }

        for (int step = 0; step < 10; step++)
        {
            polymer = ApplyInsertion(polymer, rules);
        }

        Dictionary<char, int> counts = CountElements(polymer);
        int min = counts.Min(c => c.Value);
        int max = counts.Max(c => c.Value);

        Console.WriteLine(max - min);
    }

    static string ApplyInsertion(string polymer, Dictionary<string, string> rules)
    {
        string newPolymer = "";
        for (int i = 0; i < polymer.Length - 1; i++)
        {
            newPolymer += polymer[i];
            if (rules.TryGetValue(polymer.Substring(i, 2), out string insert))
            {
                newPolymer += insert;
            }
        }
        newPolymer += polymer[polymer.Length - 1];
        return newPolymer;
    }

    static Dictionary<char, int> CountElements(string polymer)
    {
        Dictionary<char, int> counts = new Dictionary<char, int>();
        foreach (char c in polymer)
        {
            if (counts.ContainsKey(c))
            {
                counts[c]++;
            }
            else
            {
                counts[c] = 1;
            }
        }
        return counts;
    }
}
