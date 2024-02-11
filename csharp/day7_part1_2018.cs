
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var (deps, allSteps) = ParseInput("input.txt");
        var order = TopologicalSort(deps, allSteps);
        Console.WriteLine(order);
    }

    static (Dictionary<char, List<char>>, Dictionary<char, bool>) ParseInput(string filename)
    {
        var deps = new Dictionary<char, List<char>>();
        var allSteps = new Dictionary<char, bool>();

        using (StreamReader sr = new StreamReader(filename))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                char a = line[5];
                char b = line[36];
                if (!deps.ContainsKey(b))
                {
                    deps[b] = new List<char>();
                }
                deps[b].Add(a);
                allSteps[a] = true;
                allSteps[b] = true;
            }
        }

        return (deps, allSteps);
    }

    static string TopologicalSort(Dictionary<char, List<char>> deps, Dictionary<char, bool> allSteps)
    {
        var order = new List<char>();
        var available = new List<char>();

        foreach (var step in allSteps.Keys)
        {
            if (!deps.ContainsKey(step) || deps[step].Count == 0)
            {
                available.Add(step);
            }
        }
        available.Sort();

        while (available.Count > 0)
        {
            char next = available[0];
            available.RemoveAt(0);
            order.Add(next);

            foreach (var step in allSteps.Keys)
            {
                if (deps.ContainsKey(step) && deps[step].Contains(next))
                {
                    deps[step].Remove(next);
                    if (deps[step].Count == 0)
                    {
                        available.Add(step);
                    }
                }
            }
            available.Sort();
        }

        return new string(order.ToArray());
    }
}
