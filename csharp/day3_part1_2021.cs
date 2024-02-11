
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[,] counts = new int[12, 2];

        foreach (string line in lines)
        {
            for (int i = 0; i < line.Length; i++)
            {
                counts[i, line[i] - '0']++;
            }
        }

        int gammaRate = 0;
        int epsilonRate = 0;
        for (int i = 0; i < counts.GetLength(0); i++)
        {
            if (counts[i, 0] > counts[i, 1])
            {
                gammaRate |= 1 << (counts.GetLength(0) - i - 1);
            }
            else
            {
                epsilonRate |= 1 << (counts.GetLength(0) - i - 1);
            }
        }

        Console.WriteLine(gammaRate * epsilonRate);
    }
}
