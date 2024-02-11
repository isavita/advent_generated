
using System;
using System.Collections.Generic;
using System.IO;

class Node
{
    public string Name;
    public List<Node> Children = new List<Node>();
    public Node Parent;
}

class Program
{
    static Node FindOrCreateNode(string name, Dictionary<string, Node> nodes)
    {
        if (nodes.ContainsKey(name))
        {
            return nodes[name];
        }
        var node = new Node { Name = name };
        nodes[name] = node;
        return node;
    }

    static Dictionary<string, Node> BuildOrbitMap(string[] lines)
    {
        var nodes = new Dictionary<string, Node>();
        foreach (var line in lines)
        {
            var parts = line.Split(')');
            var center = FindOrCreateNode(parts[0], nodes);
            var orbiter = FindOrCreateNode(parts[1], nodes);
            center.Children.Add(orbiter);
            orbiter.Parent = center;
        }
        return nodes;
    }

    static List<Node> PathToRoot(Node node)
    {
        var path = new List<Node>();
        while (node != null)
        {
            path.Add(node);
            node = node.Parent;
        }
        return path;
    }

    static Tuple<int, int> FindCommonAncestor(Node node1, Node node2)
    {
        var path1 = PathToRoot(node1);
        var path2 = PathToRoot(node2);

        int i = path1.Count - 1;
        int j = path2.Count - 1;

        while (i >= 0 && j >= 0 && path1[i] == path2[j])
        {
            i--;
            j--;
        }
        return Tuple.Create(i + 1, j + 1);
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        var orbitMap = BuildOrbitMap(lines);

        var transfers = FindCommonAncestor(orbitMap["YOU"].Parent, orbitMap["SAN"].Parent);
        Console.WriteLine(transfers.Item1 + transfers.Item2);
    }
}
