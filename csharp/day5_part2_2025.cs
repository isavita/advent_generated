
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Cafeteria
{
    public static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("Error: Could not read input.txt");
            return;
        }

        var lines = File.ReadAllLines("input.txt");

        var (mergedRanges, availableIds) = ParseInput(lines);

        if (!mergedRanges.Any())
        {
            return;
        }

        var freshAvailableCount = availableIds.Count(id => mergedRanges.Any(r => id >= r.Start && id <= r.End));

        var totalFreshIds = mergedRanges.Sum(r => r.End - r.Start + 1);

        Console.WriteLine($"Part One: {freshAvailableCount}");
        Console.WriteLine($"Part Two: {totalFreshIds}");
    }

    private static (List<Range>, List<long>) ParseInput(string[] lines)
    {
        var rawRanges = new List<Range>();
        var availableIds = new List<long>();
        var parsingRanges = true;

        foreach (var line in lines.Select(l => l.Trim()).Where(l => l != ""))
        {
            if (parsingRanges && line.Contains("-"))
            {
                var parts = line.Split('-');
                rawRanges.Add(new Range(long.Parse(parts[0]), long.Parse(parts[1])));
            }
            else
            {
                parsingRanges = false;
                availableIds.Add(long.Parse(line));
            }
        }

        var mergedRanges = rawRanges.OrderBy(r => r.Start)
                                    .Aggregate(new List<Range>(), (list, range) =>
                                    {
                                        if (list.Count == 0 || range.Start > list.Last().End)
                                        {
                                            list.Add(range);
                                        }
                                        else
                                        {
                                            list[list.Count - 1] = new Range(list.Last().Start, Math.Max(list.Last().End, range.End));
                                        }
                                        return list;
                                    });

        return (mergedRanges, availableIds);
    }
}

public class Range
{
    public long Start { get; }
    public long End { get; }

    public Range(long start, long end)
    {
        Start = start;
        End = end;
    }
}
