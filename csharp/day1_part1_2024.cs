
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var left = new int[lines.Length];
        var right = new int[lines.Length];

        for (int i = 0; i < lines.Length; i++)
        {
            var parts = lines[i].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            left[i] = int.Parse(parts[0]);
            right[i] = int.Parse(parts[1]);
        }

        Array.Sort(left);
        Array.Sort(right);

        long total = 0;
        for (int i = 0; i < left.Length; i++)
            total += Math.Abs(left[i] - right[i]);

        Console.WriteLine($"Total distance: {total}");
    }
}
