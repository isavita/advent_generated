
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        int safe = 0;
        foreach (var line in File.ReadLines("input.txt"))
        {
            var levels = line.Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(int.Parse).ToList();
            if (IsSafe(levels) || IsSafeAfterRemovingOne(levels))
                safe++;
        }
        Console.WriteLine(safe);
    }

    static bool IsSafe(System.Collections.Generic.IList<int> a)
    {
        if (a.Count < 2) return false;
        int d = a[1] - a[0];
        if (d == 0) return false;
        bool up = d > 0;
        for (int i = 0; i < a.Count - 1; i++)
        {
            int diff = a[i + 1] - a[i];
            if (diff == 0 || (up && diff <= 0) || (!up && diff >= 0) || Math.Abs(diff) > 3)
                return false;
        }
        return true;
    }

    static bool IsSafeAfterRemovingOne(System.Collections.Generic.IList<int> a)
    {
        for (int i = 0; i < a.Count; i++)
        {
            var t = a.Where((_, idx) => idx != i).ToList();
            if (IsSafe(t)) return true;
        }
        return false;
    }
}
