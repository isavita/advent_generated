
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static long Concat(long a, long b)
    {
        return long.Parse(a + b.ToString());
    }

    static bool Check(long target, long[] nums)
    {
        int n = nums.Length - 1;
        int combos = 1 << (n * 2);
        for (int mask = 0; mask < combos; mask++)
        {
            long curr = nums[0];
            int m = mask;
            for (int i = 0; i < n; i++)
            {
                int op = m & 3;
                m >>= 2;
                long next = nums[i + 1];
                switch (op)
                {
                    case 0: curr += next; break;
                    case 1: curr *= next; break;
                    default: curr = Concat(curr, next); break;
                }
            }
            if (curr == target) return true;
        }
        return false;
    }

    static void Main()
    {
        long sum = 0;
        foreach (var line in File.ReadLines("input.txt"))
        {
            var parts = line.Split(':', StringSplitOptions.RemoveEmptyEntries);
            long target = long.Parse(parts[0]);
            var nums = Array.ConvertAll(parts[1].Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries), long.Parse);
            if (Check(target, nums)) sum += target;
        }
        Console.WriteLine(sum);
    }
}
