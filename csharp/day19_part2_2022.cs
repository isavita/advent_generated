
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Blueprint
{
    public int Id { get; set; }
    public int OreCost { get; set; }
    public int ClayOreCost { get; set; }
    public int ObsidianOreCost { get; set; }
    public int ObsidianClayCost { get; set; }
    public int GeodeOreCost { get; set; }
    public int GeodeObsidianCost { get; set; }
}

public class State
{
    public int Ore { get; set; }
    public int Clay { get; set; }
    public int Obsidian { get; set; }
    public int Geode { get; set; }
    public int OreRobots { get; set; }
    public int ClayRobots { get; set; }
    public int ObsidianRobots { get; set; }
    public int GeodeRobots { get; set; }
    public int TimeLeft { get; set; }

    public override bool Equals(object obj)
    {
        if (obj == null || GetType() != obj.GetType()) return false;
        var state = (State)obj;
        return Ore == state.Ore && Clay == state.Clay && Obsidian == state.Obsidian && Geode == state.Geode
            && OreRobots == state.OreRobots && ClayRobots == state.ClayRobots && ObsidianRobots == state.ObsidianRobots
            && GeodeRobots == state.GeodeRobots && TimeLeft == state.TimeLeft;
    }

    public override int GetHashCode()
    {
        unchecked
        {
            var hash = 17;
            hash = hash * 23 + Ore.GetHashCode();
            hash = hash * 23 + Clay.GetHashCode();
            hash = hash * 23 + Obsidian.GetHashCode();
            hash = hash * 23 + Geode.GetHashCode();
            hash = hash * 23 + OreRobots.GetHashCode();
            hash = hash * 23 + ClayRobots.GetHashCode();
            hash = hash * 23 + ObsidianRobots.GetHashCode();
            hash = hash * 23 + GeodeRobots.GetHashCode();
            hash = hash * 23 + TimeLeft.GetHashCode();
            return hash;
        }
    }
}

class Program
{
    static void Main()
    {
        var blueprints = File.ReadAllLines("input.txt")
            .Select(line =>
            {
                var parts = line.Split(' ');
                return new Blueprint
                {
                    Id = int.Parse(parts[1].TrimEnd(':')),
                    OreCost = int.Parse(parts[6]),
                    ClayOreCost = int.Parse(parts[12]),
                    ObsidianOreCost = int.Parse(parts[18]),
                    ObsidianClayCost = int.Parse(parts[21]),
                    GeodeOreCost = int.Parse(parts[27]),
                    GeodeObsidianCost = int.Parse(parts[30])
                };
            })
            .ToList();

        var init = new State { OreRobots = 1, TimeLeft = 32 };
        var prod = 1;
        for (var i = 0; i < Math.Min(3, blueprints.Count); i++)
        {
            prod *= MaxGeode(blueprints[i], init);
        }
        Console.WriteLine(prod);
    }

    static int MaxGeode(Blueprint b, State st)
    {
        var max = 0;
        var q = new Queue<State>();
        q.Enqueue(st);
        var visited = new HashSet<State>();

        while (q.Count > 0)
        {
            var s = q.Dequeue();
            max = Math.Max(max, s.Geode);
            if (s.TimeLeft == 0) continue;

            var maxOreCost = Math.Max(Math.Max(b.OreCost, b.ClayOreCost), Math.Max(b.ObsidianOreCost, b.GeodeOreCost));
            if (s.OreRobots >= maxOreCost) s.OreRobots = maxOreCost;
            if (s.ClayRobots >= b.ObsidianClayCost) s.ClayRobots = b.ObsidianClayCost;
            if (s.ObsidianRobots >= b.GeodeObsidianCost) s.ObsidianRobots = b.GeodeObsidianCost;

            var maxOre = s.TimeLeft * maxOreCost - s.OreRobots * (s.TimeLeft - 1);
            if (s.Ore >= maxOre) s.Ore = maxOre;
            var maxClay = s.TimeLeft * b.ObsidianClayCost - s.ClayRobots * (s.TimeLeft - 1);
            if (s.Clay >= maxClay) s.Clay = maxClay;
            var maxObsidian = s.TimeLeft * b.GeodeObsidianCost - s.ObsidianRobots * (s.TimeLeft - 1);
            if (s.Obsidian >= maxObsidian) s.Obsidian = maxObsidian;

            if (visited.Contains(s)) continue;
            visited.Add(s);

            var next = new State
            {
                Ore = s.Ore + s.OreRobots,
                Clay = s.Clay + s.ClayRobots,
                Obsidian = s.Obsidian + s.ObsidianRobots,
                Geode = s.Geode + s.GeodeRobots,
                OreRobots = s.OreRobots,
                ClayRobots = s.ClayRobots,
                ObsidianRobots = s.ObsidianRobots,
                GeodeRobots = s.GeodeRobots,
                TimeLeft = s.TimeLeft - 1
            };
            q.Enqueue(next);

            if (s.Ore >= b.OreCost)
            {
                next = new State
                {
                    Ore = s.Ore - b.OreCost + s.OreRobots,
                    Clay = s.Clay + s.ClayRobots,
                    Obsidian = s.Obsidian + s.ObsidianRobots,
                    Geode = s.Geode + s.GeodeRobots,
                    OreRobots = s.OreRobots + 1,
                    ClayRobots = s.ClayRobots,
                    ObsidianRobots = s.ObsidianRobots,
                    GeodeRobots = s.GeodeRobots,
                    TimeLeft = s.TimeLeft - 1
                };
                q.Enqueue(next);
            }

            if (s.Ore >= b.ClayOreCost)
            {
                next = new State
                {
                    Ore = s.Ore - b.ClayOreCost + s.OreRobots,
                    Clay = s.Clay + s.ClayRobots,
                    Obsidian = s.Obsidian + s.ObsidianRobots,
                    Geode = s.Geode + s.GeodeRobots,
                    OreRobots = s.OreRobots,
                    ClayRobots = s.ClayRobots + 1,
                    ObsidianRobots = s.ObsidianRobots,
                    GeodeRobots = s.GeodeRobots,
                    TimeLeft = s.TimeLeft - 1
                };
                q.Enqueue(next);
            }

            if (s.Ore >= b.ObsidianOreCost && s.Clay >= b.ObsidianClayCost)
            {
                next = new State
                {
                    Ore = s.Ore - b.ObsidianOreCost + s.OreRobots,
                    Clay = s.Clay - b.ObsidianClayCost + s.ClayRobots,
                    Obsidian = s.Obsidian + s.ObsidianRobots,
                    Geode = s.Geode + s.GeodeRobots,
                    OreRobots = s.OreRobots,
                    ClayRobots = s.ClayRobots,
                    ObsidianRobots = s.ObsidianRobots + 1,
                    GeodeRobots = s.GeodeRobots,
                    TimeLeft = s.TimeLeft - 1
                };
                q.Enqueue(next);
            }

            if (s.Ore >= b.GeodeOreCost && s.Obsidian >= b.GeodeObsidianCost)
            {
                next = new State
                {
                    Ore = s.Ore - b.GeodeOreCost + s.OreRobots,
                    Clay = s.Clay + s.ClayRobots,
                    Obsidian = s.Obsidian - b.GeodeObsidianCost + s.ObsidianRobots,
                    Geode = s.Geode + s.GeodeRobots,
                    OreRobots = s.OreRobots,
                    ClayRobots = s.ClayRobots,
                    ObsidianRobots = s.ObsidianRobots,
                    GeodeRobots = s.GeodeRobots + 1,
                    TimeLeft = s.TimeLeft - 1
                };
                q.Enqueue(next);
            }
        }
        return max;
    }
}
