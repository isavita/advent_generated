
using System;
using System.IO;
using System.Linq;

class Program
{
    static string React(string polymer)
    {
        bool reactionOccurred = true;
        while (reactionOccurred)
        {
            reactionOccurred = false;
            for (int i = 0; i < polymer.Length - 1; i++)
            {
                if (polymer[i] != polymer[i + 1] && char.ToUpper(polymer[i]) == char.ToUpper(polymer[i + 1]))
                {
                    polymer = polymer.Remove(i, 2);
                    reactionOccurred = true;
                }
            }
        }
        return polymer;
    }

    static void Main()
    {
        string polymer = File.ReadAllText("input.txt").Trim();

        int minLength = polymer.Length;
        for (char unit = 'a'; unit <= 'z'; unit++)
        {
            string tempPolymer = polymer.Replace(unit.ToString(), "").Replace(char.ToUpper(unit).ToString(), "");
            string reactedPolymer = React(tempPolymer);
            if (reactedPolymer.Length < minLength)
            {
                minLength = reactedPolymer.Length;
            }
        }

        Console.WriteLine(minLength);
    }
}
