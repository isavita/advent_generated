
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] startingNumbers = File.ReadAllText("input.txt").Trim().Split(',');

        var spoken = new int[30000000];
        Array.Fill(spoken, -1);

        int lastSpoken = 0;
        for (int i = 0; i < startingNumbers.Length; i++)
        {
            if (i == startingNumbers.Length - 1)
            {
                lastSpoken = int.Parse(startingNumbers[i]);
            }
            else
            {
                int num = int.Parse(startingNumbers[i]);
                spoken[num] = i + 1;
            }
        }

        for (int turn = startingNumbers.Length + 1; turn <= 30000000; turn++)
        {
            int nextNumber = spoken[lastSpoken] == -1 ? 0 : turn - 1 - spoken[lastSpoken];
            spoken[lastSpoken] = turn - 1;
            lastSpoken = nextNumber;
        }

        Console.WriteLine(lastSpoken);
    }
}
