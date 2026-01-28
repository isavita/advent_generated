
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        string data = File.ReadAllText("input.txt");
        string[] tokens = data.Split(new[] { ' ', '\n', '\r', '\t', ',' }, StringSplitOptions.RemoveEmptyEntries);

        long[] pow10 = new long[21];
        pow10[0] = 1;
        for (int i = 1; i <= 20; i++) pow10[i] = pow10[i - 1] * 10;

        HashSet<long> ids = new HashSet<long>();

        foreach (var token in tokens)
        {
            int dashIndex = token.IndexOf('-');
            if (dashIndex <= 0) continue;

            string[] parts = token.Split('-');
            if (parts.Length < 2 || !long.TryParse(parts[0], out long a) || !long.TryParse(parts[1], out long b)) continue;

            long lo = Math.Min(a, b);
            long hi = Math.Max(a, b);

            for (int k = 1; k <= 10; k++)
            {
                long mul = pow10[k] + 1;
                long minSeed = pow10[k - 1];
                long maxSeed = pow10[k] - 1;

                long sMin = (lo + mul - 1) / mul;
                long sMax = hi / mul;

                sMin = Math.Max(sMin, minSeed);
                sMax = Math.Min(sMax, maxSeed);

                for (long seed = sMin; seed <= sMax; seed++)
                {
                    ids.Add(seed * mul);
                }
            }
        }

        long sum = 0;
        foreach (var id in ids) sum += id;
        Console.WriteLine(sum);
    }
}
