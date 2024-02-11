
using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();

        int nice = 0;
        Regex disallowPattern = new Regex("(ab|cd|pq|xy)");
        foreach (string line in input.Split('\n'))
        {
            int vowels = 0;
            foreach (char c in line)
            {
                if ("aeiou".Contains(c))
                {
                    vowels++;
                }
            }

            bool hasDouble = false;
            for (int i = 0; i < line.Length - 1; i++)
            {
                if (line[i] == line[i + 1])
                {
                    hasDouble = true;
                    break;
                }
            }

            if (vowels >= 3 && !disallowPattern.IsMatch(line) && hasDouble)
            {
                nice++;
            }
        }

        Console.WriteLine(nice);
    }
}
