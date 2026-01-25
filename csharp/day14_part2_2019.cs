
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class ChemicalReaction
{
    public Dictionary<string, long> Inputs { get; set; }
    public string OutputName { get; set; }
    public long OutputAmount { get; set; }

    public ChemicalReaction()
    {
        Inputs = new Dictionary<string, long>();
    }
}

public class Program
{
    private static Dictionary<string, ChemicalReaction> reactions = new Dictionary<string, ChemicalReaction>();
    private static Dictionary<string, long> surplus = new Dictionary<string, long>();

    public static void Main(string[] args)
    {
        var lines = File.ReadAllLines("input.txt");
        ParseReactions(lines);

        if (!reactions.ContainsKey("FUEL"))
        {
            Console.WriteLine("Error: FUEL reaction not found in input");
            return;
        }

        var oreAvailable = 1000000000000L;
        var low = 0L;
        var high = oreAvailable;

        // Estimate a reasonable upper bound for high
        surplus.Clear();
        var oreForOneFuel = CalculateOre("FUEL", 1);
        if (oreForOneFuel > 0)
        {
            high = (oreAvailable / oreForOneFuel) * 2;
            if (high == 0) high = 1;
        }

        var maxFuel = 0L;
        while (low <= high)
        {
            var mid = low + (high - low) / 2;
            if (mid == 0)
            {
                low = 1;
                continue;
            }

            surplus.Clear();
            var oreNeeded = CalculateOre("FUEL", mid);

            if (oreNeeded <= oreAvailable)
            {
                maxFuel = mid;
                low = mid + 1;
            }
            else
            {
                high = mid - 1;
            }
        }

        Console.WriteLine(maxFuel);
    }

    private static void ParseReactions(string[] lines)
    {
        foreach (var line in lines)
        {
            var parts = line.Trim().Split(new[] { "=>" }, StringSplitOptions.None);
            var inputs = parts[0].Trim().Split(',');
            var output = parts[1].Trim().Split(' ');

            var reaction = new ChemicalReaction
            {
                OutputName = output[1],
                OutputAmount = long.Parse(output[0])
            };

            foreach (var input in inputs)
            {
                var inputParts = input.Trim().Split(' ');
                reaction.Inputs.Add(inputParts[1], long.Parse(inputParts[0]));
            }

            reactions[reaction.OutputName] = reaction;
        }
    }

    private static long CalculateOre(string chemical, long amount)
    {
        if (chemical == "ORE")
        {
            return amount;
        }

        if (surplus.ContainsKey(chemical) && surplus[chemical] >= amount)
        {
            surplus[chemical] -= amount;
            return 0;
        }

        amount -= surplus.ContainsKey(chemical) ? surplus[chemical] : 0;
        surplus[chemical] = 0;

        if (!reactions.TryGetValue(chemical, out var reaction))
        {
            Console.WriteLine($"Error: Reaction not found for {chemical}");
            return 0;
        }

        var reactionOutputAmount = reaction.OutputAmount;
        var times = (amount + reactionOutputAmount - 1) / reactionOutputAmount;

        long oreNeeded = 0;
        foreach (var input in reaction.Inputs)
        {
            oreNeeded += CalculateOre(input.Key, input.Value * times);
        }

        surplus[chemical] = times * reactionOutputAmount - amount;
        return oreNeeded;
    }
}
