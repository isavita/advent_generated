
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int target = int.Parse(input) / 11;

        int[] houses = new int[target + 1];

        for (int elf = 1; elf <= target; elf++)
        {
            for (int house = elf; house <= elf * 50 && house <= target; house += elf)
            {
                houses[house] += elf;
            }
        }

        for (int houseNumber = 0; houseNumber < houses.Length; houseNumber++)
        {
            if (houses[houseNumber] >= target)
            {
                Console.WriteLine(houseNumber);
                break;
            }
        }
    }
}
