
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void DFS(int node, Dictionary<int, List<int>> adj, Dictionary<int, bool> visited)
    {
        visited[node] = true;
        foreach (var neighbor in adj[node])
        {
            if (!visited.ContainsKey(neighbor) || !visited[neighbor])
            {
                DFS(neighbor, adj, visited);
            }
        }
    }

    static void Main()
    {
        Dictionary<int, List<int>> adj = new Dictionary<int, List<int>>();
        Dictionary<int, bool> visited = new Dictionary<int, bool>();
        int groups = 0;

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                string[] parts = line.Split(" <-> ");
                int from = int.Parse(parts[0]);
                string[] toNodes = parts[1].Split(", ");

                if (!adj.ContainsKey(from))
                {
                    adj[from] = new List<int>();
                }

                foreach (string toNode in toNodes)
                {
                    int to = int.Parse(toNode);
                    adj[from].Add(to);

                    if (!adj.ContainsKey(to))
                    {
                        adj[to] = new List<int>();
                    }
                    adj[to].Add(from);
                }
            }
        }

        foreach (int node in adj.Keys)
        {
            if (!visited.ContainsKey(node) || !visited[node])
            {
                DFS(node, adj, visited);
                groups++;
            }
        }

        Console.WriteLine(groups);
    }
}
