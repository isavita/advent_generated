
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        int[][] slopes = new int[][]
        {
            new int[] {1, 1},
            new int[] {3, 1},
            new int[] {5, 1},
            new int[] {7, 1},
            new int[] {1, 2}
        };

        long product = 1;
        foreach (var slope in slopes)
        {
            int treeCount = 0;
            int pos = 0;
            for (int i = 0; i < lines.Length; i += slope[1])
            {
                if (lines[i][pos] == '#')
                {
                    treeCount++;
                }
                pos = (pos + slope[0]) % lines[i].Length;
            }
            product *= treeCount;
        }

        Console.WriteLine(product);
    }
}
