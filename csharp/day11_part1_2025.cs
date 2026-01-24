
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static Dictionary<string, List<string>> graph = new Dictionary<string, List<string>>();
    static Dictionary<string, int> memo = new Dictionary<string, int>();

    static int Dfs(string u, string target)
    {
        if (u == target) return 1;
        if (memo.ContainsKey(u)) return memo[u];
        int total = 0;
        if (graph.ContainsKey(u))
            foreach (var v in graph[u])
                total += Dfs(v, target);
        memo[u] = total;
        return total;
    }

    static void Main()
    {
        foreach (var line in File.ReadLines("input.txt"))
        {
            var parts = line.Split(':');
            if (parts.Length < 2) continue;
            var src = parts[0].Trim();
            if (!graph.ContainsKey(src)) graph[src] = new List<string>();
            foreach (var tok in parts[1].Split(' ', StringSplitOptions.RemoveEmptyEntries))
                graph[src].Add(tok);
        }
        Console.WriteLine(Dfs("you", "out"));
    }
}
