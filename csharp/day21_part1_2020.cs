
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Solution
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var allergenMap = new Dictionary<string, Dictionary<string, bool>>();
        var ingredientCount = new Dictionary<string, int>();
        var safeIngredients = new Dictionary<string, bool>();

        foreach (var line in lines)
        {
            var parts = line.Split(" (contains ");
            var ingredients = parts[0].Split(' ');
            var allergens = parts.Length > 1 ? parts[1].Substring(0, parts[1].Length - 1).Split(", ") : new string[0];

            foreach (var ingredient in ingredients)
            {
                if (!ingredientCount.ContainsKey(ingredient))
                {
                    ingredientCount[ingredient] = 1;
                    safeIngredients[ingredient] = true;
                }
                else
                {
                    ingredientCount[ingredient]++;
                }
            }

            foreach (var allergen in allergens)
            {
                if (!allergenMap.ContainsKey(allergen))
                {
                    allergenMap[allergen] = new Dictionary<string, bool>();
                    foreach (var ingredient in ingredients)
                    {
                        allergenMap[allergen][ingredient] = true;
                    }
                }
                else
                {
                    foreach (var ingredient in allergenMap[allergen].Keys.ToList())
                    {
                        if (!ingredients.Contains(ingredient))
                        {
                            allergenMap[allergen].Remove(ingredient);
                        }
                    }
                }
            }
        }

        foreach (var ingredients in allergenMap.Values)
        {
            foreach (var ingredient in ingredients.Keys)
            {
                safeIngredients.Remove(ingredient);
            }
        }

        int count = 0;
        foreach (var ingredient in safeIngredients.Keys)
        {
            count += ingredientCount[ingredient];
        }

        Console.WriteLine(count);
    }
}
