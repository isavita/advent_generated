
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();

        int nice = 0;
        Func<string, bool> passesRule1 = line =>
        {
            for (int i = 0; i < line.Length - 2; i++)
            {
                string toMatch = line.Substring(i, 2);
                for (int j = i + 2; j < line.Length - 1; j++)
                {
                    if (line.Substring(j, 2) == toMatch)
                    {
                        return true;
                    }
                }
            }
            return false;
        };

        foreach (string line in input.Split('\n'))
        {
            bool rule1 = passesRule1(line);

            bool rule2 = false;
            for (int i = 0; i < line.Length - 2; i++)
            {
                if (line[i] == line[i + 2])
                {
                    rule2 = true;
                    break;
                }
            }

            if (rule1 && rule2)
            {
                nice++;
            }
        }

        Console.WriteLine(nice);
    }
}
