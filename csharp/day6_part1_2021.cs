
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        int[] fishes = new int[9];

        string[] fishStrs = input.Split(',');
        foreach (var fishStr in fishStrs)
        {
            int fish = int.Parse(fishStr);
            fishes[fish]++;
        }

        for (int day = 1; day <= 80; day++)
        {
            int newFish = fishes[0];
            for (int i = 1; i < fishes.Length; i++)
            {
                fishes[i - 1] = fishes[i];
            }
            fishes[6] += newFish;
            fishes[8] = newFish;
        }

        int totalFish = fishes.Sum();

        Console.WriteLine(totalFish);
    }
}
