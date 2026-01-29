
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        if (!File.Exists("input.txt")) return;

        var stones = File.ReadAllText("input.txt")
            .Split((char[])null, StringSplitOptions.RemoveEmptyEntries)
            .Select(long.Parse)
            .GroupBy(n => n)
            .ToDictionary(g => g.Key, g => g.LongCount());

        for (int i = 0; i < 75; i++)
        {
            var next = new Dictionary<long, long>();
            foreach (var kvp in stones)
            {
                long v = kvp.Key, count = kvp.Value;
                if (v == 0)
                {
                    Add(next, 1, count);
                }
                else
                {
                    string s = v.ToString();
                    if (s.Length % 2 == 0)
                    {
                        Add(next, long.Parse(s.Substring(0, s.Length / 2)), count);
                        Add(next, long.Parse(s.Substring(s.Length / 2)), count);
                    }
                    else
                    {
                        Add(next, v * 2024, count);
                    }
                }
            }
            stones = next;
        }

        Console.WriteLine(stones.Values.Sum());
    }

    static void Add(Dictionary<long, long> dict, long key, long count)
    {
        dict[key] = dict.GetValueOrDefault(key) + count;
    }
}

