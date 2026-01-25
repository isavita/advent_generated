
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var packages = File.ReadAllLines("input.txt").Select(long.Parse).ToArray();
        long target = packages.Sum() / 4;
        int n = packages.Length;
        long bestQE = long.MaxValue;
        int bestLen = int.MaxValue;

        for (uint mask = 1; mask < (1U << n); mask++)
        {
            long w = 0, qe = 1; int c = 0;
            for (int i = 0; i < n; i++)
                if ((mask & (1U << i)) != 0) { w += packages[i]; qe *= packages[i]; c++; }

            if (w != target || c > bestLen) continue;
            var rem = Enumerable.Range(0, n).Where(i => (mask & (1U << i)) == 0).Select(i => packages[i]).ToArray();
            bool ok = false;
            for (uint m2 = 1; m2 < (1U << rem.Length); m2++)
            {
                long w2 = 0;
                for (int i = 0; i < rem.Length; i++) if ((m2 & (1U << i)) != 0) w2 += rem[i];
                if (w2 != target) continue;
                for (uint m3 = 1; m3 < (1U << rem.Length); m3++)
                {
                    if ((m2 & m3) != 0) continue;
                    long w3 = 0;
                    for (int i = 0; i < rem.Length; i++) if ((m3 & (1U << i)) != 0) w3 += rem[i];
                    if (w3 == target) { ok = true; goto outer; }
                }
            }
            outer:
            if (ok && (c < bestLen || qe < bestQE)) { bestLen = c; bestQE = qe; }
        }
        Console.WriteLine(bestQE);
    }
}
