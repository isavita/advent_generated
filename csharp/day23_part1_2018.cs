
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Linq;

class Nanobot
{
    public int X, Y, Z, Radius;
}

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        var nanobots = ParseNanobots(lines);

        var strongest = FindStrongestNanobot(nanobots);
        var inRangeCount = CountNanobotsInRange(nanobots, strongest);

        Console.WriteLine(inRangeCount);
    }

    static Nanobot[] ParseNanobots(string[] lines)
    {
        var nanobots = new Nanobot[lines.Length];
        var re = new Regex(@"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)");

        for (int i = 0; i < lines.Length; i++)
        {
            var matches = re.Match(lines[i]).Groups;

            nanobots[i] = new Nanobot
            {
                X = int.Parse(matches[1].Value),
                Y = int.Parse(matches[2].Value),
                Z = int.Parse(matches[3].Value),
                Radius = int.Parse(matches[4].Value)
            };
        }

        return nanobots;
    }

    static Nanobot FindStrongestNanobot(Nanobot[] nanobots)
    {
        return nanobots.OrderByDescending(n => n.Radius).First();
    }

    static int CountNanobotsInRange(Nanobot[] nanobots, Nanobot strongest)
    {
        return nanobots.Count(n => ManhattanDistance(n, strongest) <= strongest.Radius);
    }

    static int ManhattanDistance(Nanobot a, Nanobot b)
    {
        return Math.Abs(a.X - b.X) + Math.Abs(a.Y - b.Y) + Math.Abs(a.Z - b.Z);
    }
}
