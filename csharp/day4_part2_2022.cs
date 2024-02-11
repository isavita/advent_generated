
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int count = 0;

        foreach (string line in lines)
        {
            string[] pair = line.Split(',');

            // Extract ranges
            int[] left = ParseRange(pair[0]);
            int[] right = ParseRange(pair[1]);

            // Check if ranges overlap
            if (left[0] <= right[1] && left[1] >= right[0])
            {
                count++;
            }
        }

        Console.WriteLine(count);
    }

    static int[] ParseRange(string s)
    {
        string[] split = s.Split('-');
        int start = int.Parse(split[0]);
        int end = int.Parse(split[1]);
        return new int[] { start, end };
    }
}
