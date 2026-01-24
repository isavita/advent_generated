using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var mfcsam = new Dictionary<string, int>
        {
            ["children"] = 3,
            ["cats"] = 7,
            ["samoyeds"] = 2,
            ["pomeranians"] = 3,
            ["akitas"] = 0,
            ["vizslas"] = 0,
            ["goldfish"] = 5,
            ["trees"] = 3,
            ["cars"] = 2,
            ["perfumes"] = 1
        };

        foreach (var raw in File.ReadLines("input.txt"))
        {
            var line = raw.Replace(":", " ").Replace(",", " ");
            var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            var sue = parts[1];
            bool matches = true;
            for (int i = 2; i < parts.Length; i += 2)
            {
                var name = parts[i];
                var count = int.Parse(parts[i + 1]);
                if (!mfcsam.TryGetValue(name, out var expected) || expected != count)
                {
                    matches = false;
                    break;
                }
            }
            if (matches)
            {
                Console.WriteLine(sue);
                return;
            }
        }
    }
}