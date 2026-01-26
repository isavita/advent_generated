
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public class Solution
{
    public class Network
    {
        public string Instructions { get; set; }
        public Dictionary<string, string[]> Nodes { get; set; }

        public Network(string instructions, Dictionary<string, string[]> nodes)
        {
            Instructions = instructions;
            Nodes = nodes;
        }
    }

    public static Network ParseInput(List<string> input)
    {
        string instructions = input[0];
        var nodes = new Dictionary<string, string[]>();
        var pattern = new Regex(@"(\w+) = \((\w+), (\w+)\)");
        for (int i = 2; i < input.Count; i++)
        {
            var match = pattern.Match(input[i]);
            if (match.Success)
            {
                nodes[match.Groups[1].Value] = new[] { match.Groups[2].Value, match.Groups[3].Value };
            }
        }
        return new Network(instructions, nodes);
    }

    public static long Gcd(long a, long b)
    {
        while (b != 0)
        {
            var temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    public static long Lcm(long a, long b)
    {
        return (a * b) / Gcd(a, b);
    }

    public static long LcmSlice(List<long> nums)
    {
        if (nums.Count == 0) return 0;
        long res = nums[0];
        for (int i = 1; i < nums.Count; i++)
        {
            res = Lcm(res, nums[i]);
        }
        return res;
    }

    public static long Solve(List<string> input)
    {
        var network = ParseInput(input);
        var starts = network.Nodes.Keys.Where(node => node.EndsWith("A")).ToList();
        var steps = new List<long>();
        int instructionsLength = network.Instructions.Length;
        foreach (var start in starts)
        {
            string element = start;
            long step = 0;
            while (!element.EndsWith("Z"))
            {
                char instruction = network.Instructions[(int)(step % instructionsLength)];
                element = instruction == 'L' ? network.Nodes[element][0] : network.Nodes[element][1];
                step++;
            }
            steps.Add(step);
        }
        return LcmSlice(steps);
    }

    public static void Main(string[] args)
    {
        var input = File.ReadAllLines("input.txt").ToList();
        Console.WriteLine(Solve(input));
    }
}
