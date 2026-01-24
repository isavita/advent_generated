
using System;
using System.IO;

class Program
{
    static bool IsSafe(int[] a)
    {
        if (a.Length < 2) return false;
        int d = a[1] - a[0];
        bool inc = d > 0;
        if (d == 0) return false;
        for (int i = 0; i < a.Length - 1; i++)
        {
            int diff = a[i + 1] - a[i];
            if (diff == 0 || (inc && diff <= 0) || (!inc && diff >= 0)) return false;
            int ad = Math.Abs(diff);
            if (ad < 1 || ad > 3) return false;
        }
        return true;
    }

    static void Main()
    {
        int safe = 0;
        foreach (var line in File.ReadLines("input.txt"))
        {
            var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            int[] a = Array.ConvertAll(parts, int.Parse);
            if (IsSafe(a)) safe++;
        }
        Console.WriteLine(safe);
    }
}
