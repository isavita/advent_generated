
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Graph
{
    private Dictionary<string, int> nodeIndices = new Dictionary<string, int>();
    private List<string> nodes = new List<string>();
    private bool[,] adjMatrix;

    public void AddEdge(string from, string to)
    {
        int fromIndex = GetOrAddNode(from);
        int toIndex = GetOrAddNode(to);
        adjMatrix[fromIndex, toIndex] = true;
        adjMatrix[toIndex, fromIndex] = true;
    }

    private int GetOrAddNode(string name)
    {
        if (nodeIndices.TryGetValue(name, out int index))
            return index;

        nodes.Add(name);
        nodeIndices[name] = nodes.Count - 1;
        if (adjMatrix == null)
            adjMatrix = new bool[nodes.Count, nodes.Count];
        else if (nodes.Count > adjMatrix.GetLength(0))
        {
            bool[,] newMatrix = new bool[nodes.Count, nodes.Count];
            for (int i = 0; i < adjMatrix.GetLength(0); i++)
                for (int j = 0; j < adjMatrix.GetLength(1); j++)
                    newMatrix[i, j] = adjMatrix[i, j];
            adjMatrix = newMatrix;
        }
        return nodes.Count - 1;
    }

    public int Walk(bool doubleUsed)
    {
        int[] visited = new int[nodes.Count];
        int startNode = GetOrAddNode("start");
        return Walk(startNode, visited, doubleUsed);
    }

    private int Walk(int current, int[] visited, bool doubleUsed)
    {
        if (nodes[current] == "end") return 1;
        visited[current]++;
        int pathsToEnd = 0;

        for (int next = 0; next < nodes.Count; ++next)
        {
            if (!adjMatrix[current, next]) continue;
            if (nodes[next] == "start") continue;
            if (IsLowerCase(nodes[next]) && visited[next] > 0)
            {
                if (doubleUsed) continue;
                else doubleUsed = true;
            }

            pathsToEnd += Walk(next, visited, doubleUsed);
            visited[next]--;

            if (IsLowerCase(nodes[next]) && visited[next] == 1)
                doubleUsed = false;
        }
        return pathsToEnd;
    }

    private bool IsLowerCase(string str)
    {
        return str.All(char.IsLower);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string[] lines = File.ReadAllLines("input.txt");
        Graph graph = new Graph();

        foreach (var line in lines)
        {
            string[] parts = line.Split('-');
            graph.AddEdge(parts[0].Trim(), parts[1].Trim());
        }

        Console.WriteLine(graph.Walk(false));
    }
}
