
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] data = File.ReadAllText("input.txt").Split(new char[] { ' ', '\n', '\r', '\t' }, StringSplitOptions.RemoveEmptyEntries);
        int[] banks = data.Select(int.Parse).ToArray();

        var seen = new System.Collections.Generic.Dictionary<string, int>();
        int cycles = 0;

        while (true)
        {
            string state = string.Join(",", banks);

            if (seen.ContainsKey(state))
            {
                Console.WriteLine("The size of the loop is " + (cycles - seen[state]));
                return;
            }
            seen[state] = cycles;

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
    }
}
