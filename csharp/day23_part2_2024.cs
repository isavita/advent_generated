
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    const string FILENAME = "input.txt";
    const int MAX_NODES = 2048;

    static string[] nodeNames = new string[MAX_NODES];
    static int nodeCount = 0;
    static bool[,] adj = new bool[MAX_NODES, MAX_NODES];

    static List<int> maxCliqueNodes = new List<int>();

    static void Main()
    {
        // read input
        foreach (var line in File.ReadLines(FILENAME))
        {
            var parts = line.Split('-');
            if (parts.Length != 2) continue;
            int a = GetNodeId(parts[0]);
            int b = GetNodeId(parts[1]);
            adj[a, b] = true;
            adj[b, a] = true;
        }

        if (nodeCount == 0) return;

        var P = Enumerable.Range(0, nodeCount).ToList();
        var R = new List<int>();
        FindMaxCliqueRecursive(R, P);

        var result = maxCliqueNodes.Select(i => nodeNames[i]).OrderBy(s => s).ToArray();
        Console.WriteLine(string.Join(",", result));
    }

    static int GetNodeId(string name)
    {
        for (int i = 0; i < nodeCount; i++)
            if (nodeNames[i] == name) return i;
        nodeNames[nodeCount] = name;
        return nodeCount++;
    }

    static void FindMaxCliqueRecursive(List<int> R, List<int> P)
    {
        if (P.Count == 0)
        {
            if (R.Count > maxCliqueNodes.Count)
            {
                maxCliqueNodes = new List<int>(R);
            }
            return;
        }
        if (R.Count + P.Count <= maxCliqueNodes.Count) return;

        var copyP = new List<int>(P);
        for (int i = 0; i < copyP.Count; i++)
        {
            int v = copyP[i];
            var newR = new List<int>(R) { v };
            var newP = new List<int>();
            for (int j = i + 1; j < copyP.Count; j++)
            {
                int u = copyP[j];
                if (adj[v, u]) newP.Add(u);
            }
            FindMaxCliqueRecursive(newR, newP);
        }
    }
}
