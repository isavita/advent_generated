
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Rule
{
    public int X { get; set; }
    public int Y { get; set; }
}

public class Program
{
    private const int MaxPosValue = 65535;

    public static void Main(string[] args)
    {
        var rules = new List<Rule>();
        var updates = new List<int[]>();

        var lines = File.ReadAllLines("input.txt");
        bool parsingRules = true;

        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line))
                continue;

            if (parsingRules && line.Contains('|'))
            {
                var parts = line.Split('|');
                if (parts.Length == 2 && int.TryParse(parts[0], out int x) && int.TryParse(parts[1], out int y))
                {
                    rules.Add(new Rule { X = x, Y = y });
                }
            }
            else
            {
                parsingRules = false;
                var update = line.Split(',').Select(int.Parse).ToArray();
                updates.Add(update);
            }
        }

        long totalSum = 0;
        foreach (var update in updates)
        {
            if (IsCorrect(update, rules))
            {
                totalSum += update[update.Length / 2];
            }
        }

        Console.WriteLine(totalSum);
    }

    private static bool IsCorrect(int[] update, List<Rule> rules)
    {
        var posLookup = new int[MaxPosValue + 1];
        for (int i = 0; i < posLookup.Length; i++)
        {
            posLookup[i] = -1;
        }

        for (int i = 0; i < update.Length; i++)
        {
            if (update[i] >= 0 && update[i] <= MaxPosValue)
            {
                posLookup[update[i]] = i;
            }
        }

        foreach (var rule in rules)
        {
            int posX = rule.X >= 0 && rule.X <= MaxPosValue ? posLookup[rule.X] : -1;
            int posY = rule.Y >= 0 && rule.Y <= MaxPosValue ? posLookup[rule.Y] : -1;

            if (posX != -1 && posY != -1 && posX > posY)
            {
                return false;
            }
        }

        return true;
    }
}
