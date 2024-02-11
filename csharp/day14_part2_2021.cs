
using System;
using System.Collections.Generic;
using System.IO;

class Solution
{
    static void Main()
    {
        string[] input = File.ReadAllLines("input.txt");
        string template = input[0];
        Dictionary<string, string> rules = new Dictionary<string, string>();

        for (int i = 1; i < input.Length; i++)
        {
            if (string.IsNullOrEmpty(input[i]))
                continue;

            string[] parts = input[i].Split(" -> ");
            rules[parts[0]] = parts[1];
        }

        Dictionary<string, long> pairCounts = new Dictionary<string, long>();
        for (int i = 0; i < template.Length - 1; i++)
        {
            string pair = template.Substring(i, 2);
            if (pairCounts.ContainsKey(pair))
                pairCounts[pair]++;
            else
                pairCounts[pair] = 1;
        }

        for (int step = 0; step < 40; step++)
        {
            Dictionary<string, long> newPairCounts = new Dictionary<string, long>();
            foreach (var pairCount in pairCounts)
            {
                string pair = pairCount.Key;
                long count = pairCount.Value;

                if (rules.ContainsKey(pair))
                {
                    string insert = rules[pair];
                    newPairCounts[pair[0] + insert] = newPairCounts.GetValueOrDefault(pair[0] + insert) + count;
                    newPairCounts[insert + pair[1]] = newPairCounts.GetValueOrDefault(insert + pair[1]) + count;
                }
                else
                {
                    newPairCounts[pair] = newPairCounts.GetValueOrDefault(pair) + count;
                }
            }
            pairCounts = newPairCounts;
        }

        Dictionary<char, long> elementCounts = new Dictionary<char, long>();
        foreach (var pairCount in pairCounts)
        {
            char firstElement = pairCount.Key[0];
            long count = pairCount.Value;
            elementCounts[firstElement] = elementCounts.GetValueOrDefault(firstElement) + count;
        }
        elementCounts[template[template.Length - 1]]++;

        long maxCount = 0;
        long minCount = long.MaxValue;
        foreach (var count in elementCounts.Values)
        {
            maxCount = Math.Max(maxCount, count);
            minCount = Math.Min(minCount, count);
        }

        Console.WriteLine(maxCount - minCount);
    }
}
