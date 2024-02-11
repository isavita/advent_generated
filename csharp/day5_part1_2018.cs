
using System;
using System.IO;

class Program
{
    static string React(string polymer)
    {
        for (int i = 0; i < polymer.Length - 1; i++)
        {
            if (polymer[i] != polymer[i + 1] &&
                (char.ToLower(polymer[i]) == char.ToLower(polymer[i + 1])))
            {
                return React(polymer.Remove(i, 2));
            }
        }
        return polymer;
    }

    static void Main()
    {
        string polymer = File.ReadAllText("input.txt").Trim();

        string result = React(polymer);
        Console.WriteLine(result.Length);
    }
}
