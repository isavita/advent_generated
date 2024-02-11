
using System;
using System.IO;
using System.Linq;

class Ingredient
{
    public string name;
    public int capacity;
    public int durability;
    public int flavor;
    public int texture;
    public int calories;
}

class Program
{
    static void Main()
    {
        Ingredient[] ingredients = ReadIngredients("input.txt");
        if (ingredients == null)
        {
            Console.WriteLine("Error reading input");
            return;
        }

        int maxScore = FindMaxScore(ingredients, 100, 500);
        Console.WriteLine(maxScore);
    }

    static Ingredient[] ReadIngredients(string filename)
    {
        string[] lines = File.ReadAllLines(filename);
        Ingredient[] ingredients = new Ingredient[lines.Length];

        for (int i = 0; i < lines.Length; i++)
        {
            string[] parts = lines[i].Split(' ');
            if (parts.Length < 11)
            {
                continue;
            }

            int capacity = int.Parse(parts[2].Substring(0, parts[2].Length - 1));
            int durability = int.Parse(parts[4].Substring(0, parts[4].Length - 1));
            int flavor = int.Parse(parts[6].Substring(0, parts[6].Length - 1));
            int texture = int.Parse(parts[8].Substring(0, parts[8].Length - 1));
            int calories = int.Parse(parts[10]);

            ingredients[i] = new Ingredient
            {
                name = parts[0],
                capacity = capacity,
                durability = durability,
                flavor = flavor,
                texture = texture,
                calories = calories
            };
        }

        return ingredients;
    }

    static int FindMaxScore(Ingredient[] ingredients, int totalTeaspoons, int targetCalories)
    {
        return CalculateMaxScore(ingredients, 0, totalTeaspoons, Enumerable.Repeat(0, ingredients.Length).ToArray(), targetCalories);
    }

    static int CalculateMaxScore(Ingredient[] ingredients, int index, int remaining, int[] teaspoons, int targetCalories)
    {
        if (index == ingredients.Length - 1)
        {
            teaspoons[index] = remaining;
            if (CalculateCalories(ingredients, teaspoons) == targetCalories)
            {
                return Score(ingredients, teaspoons);
            }
            return 0;
        }

        int maxScore = 0;
        for (int i = 0; i <= remaining; i++)
        {
            int[] newTeaspoons = teaspoons.ToArray();
            newTeaspoons[index] = i;
            int score = CalculateMaxScore(ingredients, index + 1, remaining - i, newTeaspoons, targetCalories);
            maxScore = Math.Max(maxScore, score);
        }
        return maxScore;
    }

    static int Score(Ingredient[] ingredients, int[] teaspoons)
    {
        int capacity = 0, durability = 0, flavor = 0, texture = 0;
        for (int i = 0; i < ingredients.Length; i++)
        {
            capacity += ingredients[i].capacity * teaspoons[i];
            durability += ingredients[i].durability * teaspoons[i];
            flavor += ingredients[i].flavor * teaspoons[i];
            texture += ingredients[i].texture * teaspoons[i];
        }

        capacity = Math.Max(0, capacity);
        durability = Math.Max(0, durability);
        flavor = Math.Max(0, flavor);
        texture = Math.Max(0, texture);

        return capacity * durability * flavor * texture;
    }

    static int CalculateCalories(Ingredient[] ingredients, int[] teaspoons)
    {
        int calories = 0;
        for (int i = 0; i < ingredients.Length; i++)
        {
            calories += ingredients[i].calories * teaspoons[i];
        }
        return calories;
    }
}
