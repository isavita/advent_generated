
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Chemical
{
    public string Name { get; set; }
    public int Amount { get; set; }
}

class Program
{
    static Chemical ParseChemical(string s)
    {
        var parts = s.Split(' ');
        return new Chemical { Amount = int.Parse(parts[0]), Name = parts[1] };
    }

    static int CalculateOre(string chem, int amount, Dictionary<string, Chemical> reactions, Dictionary<string, List<Chemical>> ingredients, Dictionary<string, int> surplus)
    {
        if (chem == "ORE")
        {
            return amount;
        }

        if (surplus.ContainsKey(chem) && surplus[chem] >= amount)
        {
            surplus[chem] -= amount;
            return 0;
        }

        amount -= surplus.ContainsKey(chem) ? surplus[chem] : 0;
        surplus[chem] = 0;
        var reaction = reactions[chem];
        var times = (amount + reaction.Amount - 1) / reaction.Amount;
        var ore = 0;

        foreach (var ingredient in ingredients[chem])
        {
            ore += CalculateOre(ingredient.Name, ingredient.Amount * times, reactions, ingredients, surplus);
        }

        surplus[chem] += times * reaction.Amount - amount;
        return ore;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var reactions = new Dictionary<string, Chemical>();
        var ingredients = new Dictionary<string, List<Chemical>>();
        var allChemicals = new HashSet<string>();

        foreach (var line in lines)
        {
            var parts = line.Split(" => ");
            var inputs = parts[0].Split(", ").Select(ParseChemical).ToList();
            var output = ParseChemical(parts[1]);
            reactions[output.Name] = output;
            ingredients[output.Name] = inputs;

            foreach (var input in inputs)
            {
                allChemicals.Add(input.Name);
            }
            allChemicals.Add(output.Name);
        }

        foreach (var chemical in allChemicals)
        {
            if (!reactions.ContainsKey(chemical))
            {
                reactions[chemical] = new Chemical { Name = chemical, Amount = 0 };
            }
        }

        Console.WriteLine(CalculateOre("FUEL", 1, reactions, ingredients, new Dictionary<string, int>()));
    }
}
