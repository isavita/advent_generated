
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Program
{
    class Row
    {
        public string Springs { get; set; }
        public List<int> Group { get; set; }
    }

    static List<Row> ParseInput(string[] input)
    {
        List<Row> rows = new List<Row>();
        foreach (var line in input)
        {
            var parts = line.Split(' ');
            var springs = parts[0];
            var ints = parts[1].Split(',').Select(int.Parse).ToList();

            var row = new Row
            {
                Springs = springs,
                Group = ints
            };
            rows.Add(row);
        }
        return rows;
    }

    static int CountArrangementsRecursive(Row row, int iSprings, int iGroup, int iContiguousDamaged, Dictionary<Tuple<int, int, int>, int> cache)
    {
        if (iSprings == row.Springs.Length)
        {
            if (iGroup == row.Group.Count && iContiguousDamaged == 0)
            {
                return 1;
            }
            else if (iGroup == row.Group.Count - 1 && iContiguousDamaged == row.Group[iGroup])
            {
                return 1;
            }
            return 0;
        }

        var cacheKey = Tuple.Create(iSprings, iGroup, iContiguousDamaged);
        if (cache.ContainsKey(cacheKey))
        {
            return cache[cacheKey];
        }

        var res = 0;
        var c = row.Springs[iSprings];
        if (c == '.' || c == '?')
        {
            if (iContiguousDamaged == 0)
            {
                res += CountArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache);
            }
            else if (iContiguousDamaged == row.Group[iGroup])
            {
                res += CountArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache);
            }
        }
        if (c == '#' || c == '?')
        {
            if (iGroup < row.Group.Count && iContiguousDamaged < row.Group[iGroup])
            {
                res += CountArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache);
            }
        }

        cache[cacheKey] = res;
        return res;
    }

    static int CountArrangements(Row row)
    {
        return CountArrangementsRecursive(row, 0, 0, 0, new Dictionary<Tuple<int, int, int>, int>());
    }

    static Row UnfoldRow(Row row, int unfoldingFactor)
    {
        var newRow = new Row
        {
            Springs = row.Springs,
            Group = new List<int>(row.Group)
        };

        for (int i = 1; i < unfoldingFactor; i++)
        {
            newRow.Springs += "?" + row.Springs;
            newRow.Group.AddRange(row.Group);
        }

        return newRow;
    }

    static int Solve(string[] input)
    {
        var rows = ParseInput(input);

        var res = 0;
        foreach (var row in rows)
        {
            res += CountArrangements(row);
        }

        return res;
    }

    static string[] ReadFile(string fileName)
    {
        return File.ReadAllLines(fileName);
    }

    static void Main()
    {
        var input = ReadFile("input.txt");
        Console.WriteLine(Solve(input));
    }
}
