
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Node
{
    public string Name { get; set; }
    public string Job { get; set; }
    public long Value { get; set; }
    public bool Calculated { get; set; }
}

public class Program
{
    private static Dictionary<string, Node> _nodeMap = new Dictionary<string, Node>();

    public static void Main(string[] args)
    {
        var lines = File.ReadAllLines("input.txt");
        foreach (var line in lines)
        {
            var parts = line.Split(new[] { ": " }, StringSplitOptions.None);
            var node = new Node { Name = parts[0], Job = parts[1].Trim() };
            _nodeMap[node.Name] = node;
        }

        var result = Calculate("root");
        Console.WriteLine(result);

        // No need to manually free resources in C#.
    }

    private static long Calculate(string monkey)
    {
        if (!_nodeMap.TryGetValue(monkey, out var node))
        {
            throw new Exception($"Monkey not found: {monkey}");
        }

        if (node.Calculated)
        {
            return node.Value;
        }

        if (long.TryParse(node.Job, out var num))
        {
            node.Value = num;
            node.Calculated = true;
            return num;
        }

        var parts = node.Job.Split(' ');
        var a = Calculate(parts[0]);
        var b = Calculate(parts[2]);
        long result;

        switch (parts[1])
        {
            case "+":
                result = a + b;
                break;
            case "-":
                result = a - b;
                break;
            case "*":
                result = a * b;
                break;
            case "/":
                result = a / b;
                break;
            default:
                throw new Exception($"Unknown operation: {parts[1]}");
        }

        node.Value = result;
        node.Calculated = true;
        return result;
    }
}
