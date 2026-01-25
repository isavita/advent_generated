
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Program
{
    private static Dictionary<(int, int, int), long> cache = new Dictionary<(int, int, int), long>();

    public static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("Error opening file");
            return;
        }

        long totalArrangements = 0;
        foreach (string line in File.ReadLines("input.txt"))
        {
            Row row = ParseRow(line);
            Row unfoldedRow = UnfoldRow(row, 5);
            totalArrangements += CountArrangements(unfoldedRow);
        }

        Console.WriteLine(totalArrangements);
    }

    private static Row ParseRow(string line)
    {
        string[] parts = line.Split(' ');
        string springs = parts[0];
        int[] group = parts[1].Split(',').Select(int.Parse).ToArray();
        return new Row(springs, group);
    }

    private static Row UnfoldRow(Row row, int unfoldingFactor)
    {
        string newSprings = string.Join("?", Enumerable.Repeat(row.Springs, unfoldingFactor));
        int[] newGroup = Enumerable.Repeat(row.Group, unfoldingFactor).SelectMany(g => g).ToArray();
        return new Row(newSprings, newGroup);
    }

    private static long CountArrangements(Row row)
    {
        cache.Clear();
        return CountArrangementsRecursive(row, 0, 0, 0);
    }

    private static long CountArrangementsRecursive(Row row, int iSprings, int iGroup, int iContiguousDamaged)
    {
        if (iSprings == row.Springs.Length)
        {
            return iGroup == row.Group.Length && iContiguousDamaged == 0 ||
                   iGroup == row.Group.Length - 1 && iContiguousDamaged == row.Group[iGroup] ? 1 : 0;
        }

        if (cache.TryGetValue((iSprings, iGroup, iContiguousDamaged), out long cachedValue))
        {
            return cachedValue;
        }

        long res = 0;
        char c = row.Springs[iSprings];
        if (c == '.' || c == '?')
        {
            if (iContiguousDamaged == 0)
            {
                res += CountArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged);
            }
            else if (iGroup < row.Group.Length && iContiguousDamaged == row.Group[iGroup])
            {
                res += CountArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0);
            }
        }
        if (c == '#' || c == '?')
        {
            if (iGroup < row.Group.Length && iContiguousDamaged < row.Group[iGroup])
            {
                res += CountArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1);
            }
        }

        cache[(iSprings, iGroup, iContiguousDamaged)] = res;
        return res;
    }
}

public class Row
{
    public string Springs { get; }
    public int[] Group { get; }

    public Row(string springs, int[] group)
    {
        Springs = springs;
        Group = group;
    }
}
