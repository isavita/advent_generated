
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

public class Solution
{
    public static void Main(string[] args)
    {
        var input = new List<string>();
        var steps = new List<string>();
        var stacks = new List<List<char>>();

        try
        {
            var lines = File.ReadAllLines("input.txt");
            bool isStep = false;
            foreach (var line in lines)
            {
                if (string.IsNullOrWhiteSpace(line))
                {
                    isStep = true;
                    continue;
                }
                if (!isStep)
                {
                    input.Add(line);
                }
                else
                {
                    steps.Add(line);
                }
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine(ex.Message);
        }

        var stackCount = (input.Last().Length + 1) / 4;
        for (int i = 0; i < stackCount; i++)
        {
            stacks.Add(new List<char>());
        }

        foreach (var line in input.Take(input.Count - 1))
        {
            for (int i = 1; i < line.Length; i += 4)
            {
                if (line[i] >= 'A' && line[i] <= 'Z')
                {
                    stacks[(i - 1) / 4].Insert(0, line[i]);
                }
            }
        }

        Console.WriteLine(Move(stacks, steps));
    }

    public static string Move(List<List<char>> st, List<string> steps)
    {
        var stacks = st.Select(s => new List<char>(s)).ToList();

        foreach (var step in steps)
        {
            var tokens = step.Split(' ');
            int n = int.Parse(tokens[1]);
            int from = int.Parse(tokens[3]) - 1;
            int to = int.Parse(tokens[5]) - 1;

            var count = stacks[from].Count;
            stacks[to].AddRange(stacks[from].GetRange(count - n, n));
            stacks[from].RemoveRange(count - n, n);
        }

        return string.Concat(stacks.Select(s => s.Last()));
    }
}
