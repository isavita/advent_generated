
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Rule
{
    public string Name { get; set; }
    public int Low1 { get; set; }
    public int High1 { get; set; }
    public int Low2 { get; set; }
    public int High2 { get; set; }

    public bool IsValid(int value) =>
        (value >= Low1 && value <= High1) ||
        (value >= Low2 && value <= High2);
}

public class Ticket
{
    public int[] Values { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var rules = new List<Rule>();
        var myTicket = new Ticket();
        var nearbyTickets = new List<Ticket>();

        var section = 0;
        foreach (var line in File.ReadAllLines("input.txt"))
        {
            if (string.IsNullOrWhiteSpace(line))
            {
                section++;
                continue;
            }

            switch (section)
            {
                case 0:
                    var rule = ParseRule(line);
                    if (rule != null) rules.Add(rule);
                    break;
                case 1:
                    if (!line.StartsWith("your ticket:")) myTicket = ParseTicket(line);
                    break;
                case 2:
                    if (!line.StartsWith("nearby tickets:")) nearbyTickets.Add(ParseTicket(line));
                    break;
            }
        }

        var validTickets = nearbyTickets.Where(t => t.Values.All(v => rules.Any(r => r.IsValid(v)))).ToList();

        var finalPositions = SolveFieldPositions(rules, validTickets);

        var departureProduct = 1L;
        for (var i = 0; i < rules.Count; i++)
        {
            if (rules[i].Name.StartsWith("departure"))
            {
                departureProduct *= myTicket.Values[finalPositions[i]];
            }
        }

        Console.WriteLine(departureProduct);
    }

    static Rule ParseRule(string line)
    {
        var colonIndex = line.IndexOf(':');
        if (colonIndex == -1) return null;

        var name = line.Substring(0, colonIndex);
        var ranges = line.Substring(colonIndex + 1).Trim().Split(new[] { " or " }, StringSplitOptions.None);

        if (ranges.Length != 2) return null;

        var range1 = ranges[0].Split('-');
        var range2 = ranges[1].Split('-');

        if (range1.Length != 2 || range2.Length != 2) return null;

        if (!int.TryParse(range1[0], out var low1) ||
            !int.TryParse(range1[1], out var high1) ||
            !int.TryParse(range2[0], out var low2) ||
            !int.TryParse(range2[1], out var high2)) return null;

        return new Rule { Name = name, Low1 = low1, High1 = high1, Low2 = low2, High2 = high2 };
    }

    static Ticket ParseTicket(string line)
    {
        var values = line.Split(',').Select(int.Parse).ToArray();
        return new Ticket { Values = values };
    }

    static int[] SolveFieldPositions(List<Rule> rules, List<Ticket> tickets)
    {
        var possiblePositions = new List<int>[rules.Count];
        for (var i = 0; i < rules.Count; i++)
        {
            possiblePositions[i] = Enumerable.Range(0, rules.Count).ToList();
        }

        foreach (var ticket in tickets)
        {
            for (var j = 0; j < ticket.Values.Length; j++)
            {
                for (var i = 0; i < rules.Count; i++)
                {
                    if (!rules[i].IsValid(ticket.Values[j]))
                    {
                        possiblePositions[i].Remove(j);
                    }
                }
            }
        }

        var finalPositions = new int[rules.Count];
        while (possiblePositions.Any(p => p.Count > 0))
        {
            for (var i = 0; i < rules.Count; i++)
            {
                if (possiblePositions[i].Count == 1)
                {
                    var pos = possiblePositions[i][0];
                    finalPositions[i] = pos;
                    foreach (var p in possiblePositions)
                    {
                        p.Remove(pos);
                    }
                }
            }
        }

        return finalPositions;
    }
}
