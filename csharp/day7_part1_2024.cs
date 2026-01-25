
using System;
using System.IO;
using System.Linq;

class Program
{
    static bool CanReachTarget(long target, long[] numbers)
    {
        int n = numbers.Length;
        if (n == 0) return false;
        if (n == 1) return numbers[0] == target;

        long combinations = 1L << (n - 1);
        for (long mask = 0; mask < combinations; mask++)
        {
            long result = numbers[0];
            long temp = mask;
            for (int i = 1; i < n; i++)
            {
                if ((temp & 1) == 0) result += numbers[i];
                else result *= numbers[i];
                temp >>= 1;
            }
            if (result == target) return true;
        }
        return false;
    }

    static void Main()
    {
        long total = 0;
        foreach (var rawLine in File.ReadLines("input.txt"))
        {
            var line = rawLine.Trim();
            if (string.IsNullOrEmpty(line)) continue;

            var parts = line.Split(':', 2);
            if (parts.Length != 2) continue;

            if (!long.TryParse(parts[0].Trim(), out long target)) continue;
            var nums = parts[1].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)
                                .Select(s => long.TryParse(s, out var v) ? v : 0L)
                                .ToArray();

            if (nums.Length == 0) continue;

            if (CanReachTarget(target, nums)) total += target;
        }

        Console.WriteLine(total);
    }
}
