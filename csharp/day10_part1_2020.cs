
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var adapters = lines.Select(int.Parse).ToList();
        adapters.Sort();

        var joltDifferences = new System.Collections.Generic.Dictionary<int, int> { { 3, 1 } };
        int previousJoltage = 0;

        foreach (var adapter in adapters)
        {
            int diff = adapter - previousJoltage;
            if (!joltDifferences.ContainsKey(diff))
            {
                joltDifferences[diff] = 0;
            }
            joltDifferences[diff]++;
            previousJoltage = adapter;
        }

        int product = joltDifferences[1] * joltDifferences[3];
        Console.WriteLine(product);
    }
}
