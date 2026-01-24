
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var left = new List<int>();
        var right = new List<int>();
        foreach (var line in File.ReadLines("input.txt"))
        {
            var parts = line.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length >= 2)
            {
                left.Add(int.Parse(parts[0]));
                right.Add(int.Parse(parts[1]));
            }
        }

        var freq = new Dictionary<int, int>();
        foreach (var n in right)
        {
            freq[n] = freq.GetValueOrDefault(n, 0) + 1;
        }

        long score = 0;
        foreach (var n in left)
        {
            score += n * (freq.GetValueOrDefault(n, 0));
        }

        Console.WriteLine(score);
    }
}
