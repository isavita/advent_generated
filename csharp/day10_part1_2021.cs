
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int totalScore = 0;

        foreach (string line in lines)
        {
            var result = CheckLine(line);
            if (result.Item2)
            {
                totalScore += result.Item1;
            }
        }

        Console.WriteLine(totalScore);
    }

    static Tuple<int, bool> CheckLine(string line)
    {
        Dictionary<char, char> pairings = new Dictionary<char, char>
        {
            {')', '('},
            {']', '['},
            {'}', '{'},
            {'>', '<'}
        };

        Dictionary<char, int> scores = new Dictionary<char, int>
        {
            {')', 3},
            {']', 57},
            {'}', 1197},
            {'>', 25137}
        };

        Stack<char> stack = new Stack<char>();

        foreach (char c in line)
        {
            if (pairings.ContainsValue(c))
            {
                stack.Push(c);
            }
            else if (pairings.ContainsKey(c))
            {
                if (stack.Count == 0 || stack.Peek() != pairings[c])
                {
                    return new Tuple<int, bool>(scores[c], true);
                }
                stack.Pop();
            }
        }

        return new Tuple<int, bool>(0, false);
    }
}
