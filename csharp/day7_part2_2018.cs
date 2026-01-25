using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    const int Workers = 5;
    const int Base = 60;

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var deps = new Dictionary<char, List<char>>();
        var steps = new HashSet<char>();
        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            char a = line[5], b = line[36];
            steps.Add(a);
            steps.Add(b);
            if (!deps.ContainsKey(a)) deps[a] = new List<char>();
            if (!deps.ContainsKey(b)) deps[b] = new List<char>();
            deps[b].Add(a);
        }

        var all = steps.ToList();
        var completed = new HashSet<char>();
        var inProgress = new HashSet<char>();
        var available = new SortedSet<char>(all.Where(s => deps[s].Count == 0));
        int[] remain = new int[Workers];
        char[] cur = new char[Workers];
        int time = 0;

        while (completed.Count < all.Count)
        {
            for (int i = 0; i < Workers; i++)
                if (remain[i] == 0 && available.Count > 0)
                {
                    var next = available.Min;
                    available.Remove(next);
                    cur[i] = next;
                    inProgress.Add(next);
                    remain[i] = (next - 'A') + 1 + Base;
                }

            int min = int.MaxValue;
            for (int i = 0; i < Workers; i++)
                if (remain[i] > 0 && remain[i] < min) min = remain[i];
            time += min;
            for (int i = 0; i < Workers; i++)
                if (remain[i] > 0)
                {
                    remain[i] -= min;
                    if (remain[i] == 0)
                    {
                        var finished = cur[i];
                        completed.Add(finished);
                        inProgress.Remove(finished);
                        foreach (var step in all)
                            if (!completed.Contains(step) && !inProgress.Contains(step))
                                if (deps[step].All(p => completed.Contains(p)))
                                    available.Add(step);
                        cur[i] = '\0';
                    }
                }
        }

        Console.WriteLine(time);
    }
}