
using System;
using System.Collections.Generic;
using System.IO;

struct Range
{
    public uint Start;
    public uint End;
    public Range(uint s, uint e) { Start = s; End = e; }
}

class Program
{
    static int Compare(Range a, Range b)
    {
        int c = a.Start.CompareTo(b.Start);
        return c != 0 ? c : a.End.CompareTo(b.End);
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var ranges = new List<Range>(lines.Length);
        foreach (var line in lines)
        {
            var parts = line.Split('-');
            if (parts.Length == 2 &&
                uint.TryParse(parts[0], out var s) &&
                uint.TryParse(parts[1], out var e))
                ranges.Add(new Range(s, e));
        }

        ranges.Sort(Compare);

        var merged = new List<Range>(ranges.Count);
        foreach (var r in ranges)
        {
            if (merged.Count == 0)
            {
                merged.Add(r);
                continue;
            }

            var last = merged[merged.Count - 1];
            if (last.End >= r.Start - 1)
                merged[merged.Count - 1] = new Range(last.Start, Math.Max(last.End, r.End));
            else
                merged.Add(r);
        }

        if (merged[merged.Count - 1].End != uint.MaxValue)
            merged.Add(new Range(uint.MaxValue, 0));

        ulong totalAllowed = 0;
        for (int i = 1; i < merged.Count; i++)
            totalAllowed += (ulong)merged[i].Start - merged[i - 1].End - 1;

        Console.WriteLine(totalAllowed);
    }
}
