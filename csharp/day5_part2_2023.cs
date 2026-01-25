using System;
using System.Collections.Generic;
using System.IO;

struct RangeMap
{
    public long SrcStart;
    public long DestStart;
    public long Length;
}

class Program
{
    static long ReverseConvertNumber(long number, List<RangeMap> ranges)
    {
        for (int i = ranges.Count - 1; i >= 0; i--)
        {
            var r = ranges[i];
            if (number >= r.DestStart && number < r.DestStart + r.Length)
            {
                return r.SrcStart + (number - r.DestStart);
            }
        }
        return number;
    }

    static bool IsInSeedRanges(long number, List<(long Start, long Length)> seedRanges)
    {
        foreach (var s in seedRanges)
        {
            if (number >= s.Start && number < s.Start + s.Length)
            {
                return true;
            }
        }
        return false;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var seedRanges = new List<(long Start, long Length)>();
        var maps = new List<List<RangeMap>>();
        List<RangeMap> currentMap = null;
        bool seedParsed = false;

        foreach (var raw in lines)
        {
            var line = raw.Trim();
            if (line == "") continue;
            if (!seedParsed && line.StartsWith("seeds:"))
            {
                var parts = line.Substring(6).Trim().Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                for (int i = 0; i < parts.Length; i += 2)
                {
                    long start = long.Parse(parts[i]);
                    long len = long.Parse(parts[i + 1]);
                    seedRanges.Add((start, len));
                }
                seedParsed = true;
            }
            else if (line.EndsWith("map:"))
            {
                currentMap = new List<RangeMap>();
                maps.Add(currentMap);
            }
            else
            {
                var nums = line.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                if (nums.Length == 3 && currentMap != null)
                {
                    long dest = long.Parse(nums[0]);
                    long src = long.Parse(nums[1]);
                    long len = long.Parse(nums[2]);
                    currentMap.Add(new RangeMap { SrcStart = src, DestStart = dest, Length = len });
                }
            }
        }

        long location = 0;
        while (true)
        {
            long seed = location;
            for (int i = maps.Count - 1; i >= 0; i--)
            {
                seed = ReverseConvertNumber(seed, maps[i]);
            }
            if (IsInSeedRanges(seed, seedRanges))
            {
                Console.WriteLine(location);
                break;
            }
            location++;
        }
    }
}
