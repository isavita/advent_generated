
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        var caloriesList = new int[lines.Length];
        int currentCalories = 0;
        int index = 0;

        foreach (var line in lines)
        {
            if (string.IsNullOrEmpty(line))
            {
                caloriesList[index++] = currentCalories;
                currentCalories = 0;
                continue;
            }

            currentCalories += int.Parse(line);
        }

        caloriesList[index] = currentCalories;
        Array.Sort(caloriesList);
        Array.Reverse(caloriesList);

        int topThreeSum = caloriesList.Take(3).Sum();
        Console.WriteLine(topThreeSum);
    }
}
