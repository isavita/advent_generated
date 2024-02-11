
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Node
{
    public int Used { get; set; }
    public int Avail { get; set; }
}

class Program
{
    static void Main()
    {
        List<Node> nodes = ReadNodes("input.txt");
        int viablePairs = CountViablePairs(nodes);
        Console.WriteLine(viablePairs);
    }

    static List<Node> ReadNodes(string filename)
    {
        List<Node> nodes = new List<Node>();
        string line;
        Regex nodeRegex = new Regex(@"node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%");

        using (StreamReader file = new StreamReader(filename))
        {
            while ((line = file.ReadLine()) != null)
            {
                Match match = nodeRegex.Match(line);
                if (match.Success)
                {
                    int used = int.Parse(match.Groups[1].Value);
                    int avail = int.Parse(match.Groups[2].Value);
                    nodes.Add(new Node { Used = used, Avail = avail });
                }
            }
        }

        return nodes;
    }

    static int CountViablePairs(List<Node> nodes)
    {
        int count = 0;
        for (int i = 0; i < nodes.Count; i++)
        {
            for (int j = 0; j < nodes.Count; j++)
            {
                if (i != j && nodes[i].Used > 0 && nodes[i].Used <= nodes[j].Avail)
                {
                    count++;
                }
            }
        }
        return count;
    }
}
