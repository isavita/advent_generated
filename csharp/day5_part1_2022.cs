
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var stackLines = new List<string>();
        int numStacks = 0;

        // Read stack lines
        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();
            if (string.IsNullOrEmpty(line))
                break;
            stackLines.Add(line);
        }

        numStacks = (stackLines.Last().Trim().Split(' ').Last().Trim()).ToInt();

        var stacks = new Stack<char>[numStacks];
        for (int i = 0; i < numStacks; i++)
        {
            stacks[i] = new Stack<char>();
        }

        // Populate stacks
        for (int i = stackLines.Count - 2; i >= 0; i--)
        {
            var line = stackLines[i];
            for (int j = 1; j < line.Length; j += 4)
            {
                if (char.IsLetter(line[j]))
                {
                    stacks[(j - 1) / 4].Push(line[j]);
                }
            }
        }

        // Process instructions
        for (int i = stackLines.Count; i < lines.Length; i++)
        {
            var line = lines[i].Trim();
            if (!line.StartsWith("move"))
                continue;

            var parts = line.Split(' ');
            int n = parts[1].ToInt();
            int from = parts[3].ToInt() - 1;
            int to = parts[5].ToInt() - 1;

            for (int j = 0; j < n; j++)
            {
                stacks[to].Push(stacks[from].Pop());
            }
        }

        // Print result
        Console.WriteLine(string.Join("", stacks.Select(s => s.Peek())));
    }
}

public static class Extensions
{
    public static int ToInt(this string s)
    {
        return int.Parse(s);
    }
}
