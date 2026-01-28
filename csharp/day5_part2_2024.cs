
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt").Where(l => !string.IsNullOrWhiteSpace(l));
        var rules = new HashSet<(int, int)>();
        var updates = new List<List<int>>();

        foreach (var line in lines)
        {
            if (line.Contains('|'))
            {
                var parts = line.Split('|');
                rules.Add((int.Parse(parts[0]), int.Parse(parts[1])));
            }
            else
            {
                updates.Add(line.Split(',').Select(int.Parse).ToList());
            }
        }

        int sum = 0;
        foreach (var update in updates)
        {
            if (!IsCorrect(update, rules))
            {
                update.Sort((a, b) => rules.Contains((a, b)) ? -1 : (rules.Contains((b, a)) ? 1 : 0));
                sum += update[update.Count / 2];
            }
        }
        Console.WriteLine(sum);
    }

    static bool IsCorrect(List<int> update, HashSet<(int, int)> rules)
    {
        for (int i = 0; i < update.Count; i++)
        {
            for (int j = i + 1; j < update.Count; j++)
            {
                if (rules.Contains((update[j], update[i]))) return false;
            }
        }
        return true;
    }
}
