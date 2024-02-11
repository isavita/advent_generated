
using System;
using System.IO;
using System.Text.RegularExpressions;

class Instruction
{
    public string left;
    public string right;
}

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();

        Regex re = new Regex("[A-Z]{3}");

        string[] lines = input.Split('\n');

        var desertMap = new System.Collections.Generic.Dictionary<string, Instruction>();

        for (int i = 2; i < lines.Length; i++)
        {
            if (lines[i].Length == 0)
            {
                continue;
            }

            MatchCollection matches = re.Matches(lines[i]);
            desertMap[matches[0].Value] = new Instruction
            {
                left = matches[1].Value,
                right = matches[2].Value
            };
        }

        string current = "AAA";
        int steps = 0;

        string directions = lines[0].Trim();

        while (current != "ZZZ")
        {
            foreach (char direction in directions)
            {
                if (direction == 'R')
                {
                    current = desertMap[current].right;
                }
                else if (direction == 'L')
                {
                    current = desertMap[current].left;
                }
                steps++;

                if (current == "ZZZ")
                {
                    break;
                }
            }
        }

        Console.WriteLine(steps);
    }
}
