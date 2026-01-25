
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        var input = File.ReadAllLines("input.txt");
        var food = new List<FoodItem>();
        foreach (var line in input)
        {
            var parts = line.Split(new[] { "(contains " }, StringSplitOptions.None);
            var ingredients = parts[0].Trim().Split(' ').ToList();
            var allergens = parts[1].TrimEnd(')').Split(new[] { ", " }, StringSplitOptions.None).ToList();
            food.Add(new FoodItem(ingredients, allergens));
        }

        var ingredientCounts = food.SelectMany(f => f.Ingredients).GroupBy(i => i).ToDictionary(g => g.Key, g => g.Count());
        var allergenCandidates = food.Aggregate(new Dictionary<string, HashSet<string>>(), (candidates, item) =>
        {
            foreach (var allergen in item.Allergens)
            {
                if (!candidates.TryGetValue(allergen, out var ingredientSet))
                {
                    ingredientSet = new HashSet<string>(item.Ingredients);
                    candidates[allergen] = ingredientSet;
                }
                else
                {
                    ingredientSet.IntersectWith(item.Ingredients);
                }
            }
            return candidates;
        });

        var safeIngredients = ingredientCounts.Where(ic => !allergenCandidates.Values.Any(candidates => candidates.Contains(ic.Key))).Sum(ic => ic.Value);
        Console.WriteLine(safeIngredients);

        var resolvedAllergens = ResolveAllergens(allergenCandidates);
        var canonicalList = string.Join(",", resolvedAllergens.OrderBy(a => a.Key).Select(a => a.Value));
        Console.WriteLine(canonicalList);
    }

    private static Dictionary<string, string> ResolveAllergens(Dictionary<string, HashSet<string>> allergenCandidates)
    {
        var resolvedAllergens = new Dictionary<string, string>();
        while (allergenCandidates.Count > 0)
        {
            var singleCandidate = allergenCandidates.FirstOrDefault(a => a.Value.Count == 1);
            if (singleCandidate.Key == null)
            {
                throw new InvalidOperationException("Could not resolve allergens");
            }
            var ingredient = singleCandidate.Value.First();
            resolvedAllergens[singleCandidate.Key] = ingredient;
            allergenCandidates.Remove(singleCandidate.Key);
            foreach (var candidates in allergenCandidates.Values)
            {
                candidates.Remove(ingredient);
            }
        }
        return resolvedAllergens;
    }

    private class FoodItem
    {
        public List<string> Ingredients { get; }
        public List<string> Allergens { get; }

        public FoodItem(List<string> ingredients, List<string> allergens)
        {
            Ingredients = ingredients;
            Allergens = allergens;
        }
    }
}
