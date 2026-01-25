
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
    static long Convert(long number, List<RangeMap> ranges)
    {
        foreach (var r in ranges)
            if (number >= r.SrcStart && number < r.SrcStart + r.Length)
                return r.DestStart + (number - r.SrcStart);
        return number;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var seeds = new List<long>();
        var maps = new List<List<RangeMap>>();
        var current = new List<RangeMap>();

        foreach (var raw in lines)
        {
            var line = raw.Trim();
            if (line == "") continue;
            if (line.StartsWith("seeds:"))
            {
                foreach (var part in line.Substring(6).Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries))
                    seeds.Add(long.Parse(part));
            }
            else if (line.EndsWith("map:"))
            {
                if (current.Count > 0) maps.Add(current);
                current = new List<RangeMap>();
            }
            else
            {
                var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                var destStart = long.Parse(parts[0]);
                var srcStart = long.Parse(parts[1]);
                var length = long.Parse(parts[2]);
                current.Add(new RangeMap { SrcStart = srcStart, DestStart = destStart, Length = length });
            }
        }
        if (current.Count > 0) maps.Add(current);

        long minLoc = long.MaxValue;
        foreach (var seed in seeds)
        {
            long loc = seed;
            foreach (var map in maps)
                loc = Convert(loc, map);
            if (loc < minLoc) minLoc = loc;
        }

        Console.WriteLine(minLoc);
    }
}
