using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var nameToId = new Dictionary<string, int>();
        var names = new List<string>();
        const int maxNodes = 2000;
        var adj = new bool[maxNodes, maxNodes];

        foreach (var raw in File.ReadLines("input.txt"))
        {
            var line = raw.TrimEnd('\r', '\n');
            var idx = line.IndexOf('-');
            if (idx <= 0 || idx == line.Length - 1) continue;

            var n1 = line.Substring(0, idx);
            var n2 = line.Substring(idx + 1);

            if (!nameToId.TryGetValue(n1, out var id1))
            {
                id1 = names.Count;
                if (id1 >= maxNodes) return;
                nameToId[n1] = id1;
                names.Add(n1);
            }

            if (!nameToId.TryGetValue(n2, out var id2))
            {
                id2 = names.Count;
                if (id2 >= maxNodes) return;
                nameToId[n2] = id2;
                names.Add(n2);
            }

            adj[id1, id2] = adj[id2, id1] = true;
        }

        var n = names.Count;
        long count = 0;
        for (int i = 0; i < n; ++i)
            for (int j = i + 1; j < n; ++j)
                if (adj[i, j])
                    for (int k = j + 1; k < n; ++k)
                        if (adj[i, k] && adj[j, k])
                            if (names[i][0] == 't' || names[j][0] == 't' || names[k][0] == 't')
                                ++count;

        Console.WriteLine(count);
    }
}