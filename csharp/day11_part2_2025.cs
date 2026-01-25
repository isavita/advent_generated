
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static Dictionary<string, int> id = new Dictionary<string, int>();
    static List<int>[] adj = new List<int>[0];
    static int GetId(string s)
    {
        if (!id.TryGetValue(s, out var i))
        {
            i = id.Count;
            id[s] = i;
            if (i >= adj.Length) Array.Resize(ref adj, i * 2 + 1);
            adj[i] = new List<int>();
        }
        return i;
    }
    static long Dfs(int cur, int tgt, long[] memo)
    {
        if (cur == tgt) return 1;
        if (memo[cur] != -1) return memo[cur];
        long sum = 0;
        foreach (var v in adj[cur]) sum += Dfs(v, tgt, memo);
        return memo[cur] = sum;
    }
    static long CountPaths(int s, int t)
    {
        var memo = new long[id.Count];
        for (int i = 0; i < memo.Length; i++) memo[i] = -1;
        return Dfs(s, t, memo);
    }
    static void Main()
    {
        foreach (var line in File.ReadLines("input.txt"))
        {
            var trimmed = line.Trim();
            if (trimmed.Length == 0) continue;
            var parts = trimmed.Split(':', 2);
            if (parts.Length != 2) continue;
            var src = parts[0].Trim();
            var u = GetId(src);
            foreach (var token in parts[1].Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries))
            {
                var v = GetId(token);
                adj[u].Add(v);
            }
        }
        var svr = GetId("svr");
        var dac = GetId("dac");
        var fft = GetId("fft");
        var outp = GetId("out");
        var s1 = CountPaths(svr, dac) * CountPaths(dac, fft) * CountPaths(fft, outp);
        var s2 = CountPaths(svr, fft) * CountPaths(fft, dac) * CountPaths(dac, outp);
        Console.WriteLine($"Paths (svr->dac->fft->out): {s1}");
        Console.WriteLine($"Paths (svr->fft->dac->out): {s2}");
        Console.WriteLine($"Total paths visiting both: {s1 + s2}");
    }
}
