
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] banks = lines[0].Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries)
                                .Select(int.Parse)
                                .ToArray();

        var seen = new System.Collections.Generic.HashSet<string>();
        int cycles = 0;

        while (true)
        {
            string state = string.Join(",", banks);

            if (seen.Contains(state))
            {
                break;
            }
            seen.Add(state);

            int maxIndex = 0;
            for (int i = 1; i < banks.Length; i++)
            {
                if (banks[i] > banks[maxIndex])
                {
                    maxIndex = i;
                }
            }

            int blocks = banks[maxIndex];
            banks[maxIndex] = 0;
            for (int i = 1; i <= blocks; i++)
            {
                banks[(maxIndex + i) % banks.Length]++;
            }

            cycles++;
        }

        Console.WriteLine("It takes " + cycles + " redistribution cycles to reach a repeated configuration.");
    }
}
