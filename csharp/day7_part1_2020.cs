
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Solution
{
    public static void Main()
    {
        var contains = new Dictionary<string, List<string>>();
        try
        {
            var lines = File.ReadAllLines("input.txt");
            foreach (var line in lines)
            {
                var parts = line.Split(new[] { " bags contain " }, StringSplitOptions.None);
                var container = parts[0];
                if (parts[1] == "no other bags.")
                    continue;

                var containedBags = parts[1].Split(new[] { ", " }, StringSplitOptions.None);
                foreach (var bag in containedBags)
                {
                    var bagName = string.Join(" ", bag.Split(' ').Skip(1).Take(2));
                    if (!contains.ContainsKey(bagName))
                        contains[bagName] = new List<string>();
                    contains[bagName].Add(container);
                }
            }
        }
        catch (FileNotFoundException)
        {
            Console.WriteLine("File not found.");
            return;
        }

        var count = CountCanContain("shiny gold", contains);
        Console.WriteLine(count);
    }

    private static int CountCanContain(string target, Dictionary<string, List<string>> contains)
    {
        var seen = new HashSet<string>();
        Dfs(target, contains, seen);
        return seen.Count;
    }

    private static void Dfs(string bag, Dictionary<string, List<string>> contains, HashSet<string> seen)
    {
        if (!contains.TryGetValue(bag, out var outerBags)) return;
        foreach (var outer in outerBags)
        {
            if (seen.Add(outer))
                Dfs(outer, contains, seen);
        }
    }
}
