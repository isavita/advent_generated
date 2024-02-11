
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        int maxCalories = 0;
        int currentCalories = 0;

        foreach (string line in lines)
        {
            if (line == "")
            {
                if (currentCalories > maxCalories)
                {
                    maxCalories = currentCalories;
                }
                currentCalories = 0;
                continue;
            }

            int calories = int.Parse(line);
            currentCalories += calories;
        }

        if (currentCalories > maxCalories)
        {
            maxCalories = currentCalories;
        }

        Console.WriteLine(maxCalories);
    }
}
