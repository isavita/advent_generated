
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Cafeteria
{
    public static void Main(string[] args)
    {
        try
        {
            var lines = File.ReadAllLines("input.txt");
            var ranges = new List<Range>();
            var ids = new List<long>();

            int i = 0;
            // 1. Read ranges until a blank line is encountered
            while (i < lines.Length && !string.IsNullOrWhiteSpace(lines[i]))
            {
                var parts = lines[i].Split('-');
                ranges.Add(new Range(long.Parse(parts[0]), long.Parse(parts[1])));
                i++;
            }

            // 2. Read available ingredient IDs
            while (++i < lines.Length)
            {
                if (long.TryParse(lines[i], out long id))
                {
                    ids.Add(id);
                }
            }

            // 3. Optimize ranges: Sort and Merge overlapping intervals
            var mergedRanges = MergeRanges(ranges);

            // 4. Count IDs that fall within any merged range
            var freshCount = ids.Count(id => IsFresh(id, mergedRanges));

            Console.WriteLine(freshCount);
        }
        catch (FileNotFoundException)
        {
            Console.Error.WriteLine("File not found: input.txt");
        }
    }

    private static List<Range> MergeRanges(List<Range> ranges)
    {
        if (ranges.Count == 0) return ranges;

        // Sort ranges by start ID
        ranges.Sort((x, y) => x.Start.CompareTo(y.Start));

        var merged = new List<Range> { ranges[0] };

        for (int i = 1; i < ranges.Count; i++)
        {
            var current = merged.Last();
            var next = ranges[i];
            if (next.Start <= current.End + 1)
            {
                current.End = Math.Max(current.End, next.End);
            }
            else
            {
                merged.Add(next);
            }
        }
        return merged;
    }

    private static bool IsFresh(long id, List<Range> mergedRanges)
    {
        // Binary search
        int left = 0, right = mergedRanges.Count - 1;
        while (left <= right)
        {
            int mid = left + (right - left) / 2;
            var range = mergedRanges[mid];
            if (id >= range.Start && id <= range.End) return true;
            if (id < range.Start) right = mid - 1;
            else left = mid + 1;
        }
        return false;
    }

    public class Range
    {
        public long Start { get; set; }
        public long End { get; set; }

        public Range(long start, long end)
        {
            Start = start;
            End = end;
        }
    }
}
