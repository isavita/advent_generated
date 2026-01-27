using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var openingToClosing = new Dictionary<char, char> { { '(', ')' }, { '[', ']' }, { '{', '}' }, { '<', '>' } };
        var illegalScore = new Dictionary<char, int> { { ')', 3 }, { ']', 57 }, { '}', 1197 }, { '>', 25137 } };
        var completionScore = new Dictionary<char, int> { { ')', 1 }, { ']', 2 }, { '}', 3 }, { '>', 4 } };

        long part1 = 0;
        var incompleteScores = new List<long>();

        foreach (var line in lines)
        {
            var stack = new List<char>();
            bool corrupted = false;

            foreach (var c in line)
            {
                if (openingToClosing.ContainsKey(c))
                {
                    stack.Add(c);
                }
                else
                {
                    if (stack.Count == 0 || openingToClosing[stack.Last()] != c)
                    {
                        part1 += illegalScore[c];
                        corrupted = true;
                        break;
                    }
                    stack.RemoveAt(stack.Count - 1);
                }
            }

            if (!corrupted && stack.Count > 0)
            {
                long score = 0;
                for (int i = stack.Count - 1; i >= 0; i--)
                {
                    char open = stack[i];
                    char close = openingToClosing[open];
                    score = score * 5 + completionScore[close];
                }
                incompleteScores.Add(score);
            }
        }

        incompleteScores.Sort();
        long part2 = incompleteScores[incompleteScores.Count / 2];

        Console.WriteLine(part1);
        Console.WriteLine(part2);
    }
}
