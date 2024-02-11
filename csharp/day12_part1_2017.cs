
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
            if (!visited[neighbor])
            {
                DFS(neighbor, adj, visited);
            }
        }
    }

    static void Main()
    {
        Dictionary<int, List<int>> adj = new Dictionary<int, List<int>>();
        Dictionary<int, bool> visited = new Dictionary<int, bool>();

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

                    if (!visited.ContainsKey(from))
                    {
                        visited[from] = false;
                    }
                    if (!visited.ContainsKey(to))
                    {
                        visited[to] = false;
                    }
                }
            }
        }

        DFS(0, adj, visited);

        int count = 0;
        foreach (var v in visited)
        {
            if (v.Value)
            {
                count++;
            }
        }

        Console.WriteLine(count);
    }
}
