
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        Console.WriteLine(Part1(input));
    }

    public static int Part1(string input)
    {
        var blueprints = ParseInput(input);
        int sum = 0;
        foreach (var bp in blueprints)
        {
            var st = new State(bp);
            int geodesMade = st.CalcMostGeodes(0, new Dictionary<string, int>(), 24, 24);
            sum += bp.Id * geodesMade;
        }
        return sum;
    }

    public class Blueprint
    {
        public int Id { get; set; }
        public int OreForOreRobot { get; set; }
        public int OreForClayRobot { get; set; }
        public int OreForObsidianRobot { get; set; }
        public int ClayForObsidianRobot { get; set; }
        public int OreForGeodeRobot { get; set; }
        public int ObsidianForGeodeRobot { get; set; }

        public Blueprint(int id, int oreForOreRobot, int oreForClayRobot, int oreForObsidianRobot, int clayForObsidianRobot, int oreForGeodeRobot, int obsidianForGeodeRobot)
        {
            Id = id;
            OreForOreRobot = oreForOreRobot;
            OreForClayRobot = oreForClayRobot;
            OreForObsidianRobot = oreForObsidianRobot;
            ClayForObsidianRobot = clayForObsidianRobot;
            OreForGeodeRobot = oreForGeodeRobot;
            ObsidianForGeodeRobot = obsidianForGeodeRobot;
        }
    }

    public class State
    {
        public Blueprint Blueprint { get; set; }
        public int Ore { get; set; }
        public int Clay { get; set; }
        public int Obsidian { get; set; }
        public int Geode { get; set; }
        public int OreRobots { get; set; }
        public int ClayRobots { get; set; }
        public int ObsidianRobots { get; set; }
        public int GeodeRobots { get; set; }

        public State(Blueprint blueprint)
        {
            Blueprint = blueprint;
            OreRobots = 1;
        }

        public void Farm()
        {
            Ore += OreRobots;
            Clay += ClayRobots;
            Obsidian += ObsidianRobots;
            Geode += GeodeRobots;
        }

        public string Hash(int time)
        {
            return $"{time},{Ore},{Clay},{Obsidian},{Geode},{OreRobots},{ClayRobots},{ObsidianRobots},{GeodeRobots}";
        }

        public State Copy()
        {
            var copy = new State(Blueprint)
            {
                Ore = Ore,
                Clay = Clay,
                Obsidian = Obsidian,
                Geode = Geode,
                OreRobots = OreRobots,
                ClayRobots = ClayRobots,
                ObsidianRobots = ObsidianRobots,
                GeodeRobots = GeodeRobots
            };
            return copy;
        }

        public int CalcMostGeodes(int time, Dictionary<string, int> memo, int totalTime, int earliestGeode)
        {
            if (time == totalTime)
            {
                return Geode;
            }

            var h = Hash(time);
            if (memo.ContainsKey(h))
            {
                return memo[h];
            }

            if (Geode == 0 && time > earliestGeode)
            {
                return 0;
            }

            int mostGeodes = Geode;

            if (Ore >= Blueprint.OreForGeodeRobot && Obsidian >= Blueprint.ObsidianForGeodeRobot)
            {
                var cp = Copy();
                cp.Farm();
                cp.Ore -= Blueprint.OreForGeodeRobot;
                cp.Obsidian -= Blueprint.ObsidianForGeodeRobot;
                cp.GeodeRobots++;
                if (cp.GeodeRobots == 1)
                {
                    earliestGeode = Math.Min(earliestGeode, time + 1);
                }
                mostGeodes = Math.Max(mostGeodes, cp.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode));
                memo[h] = mostGeodes;
                return mostGeodes;
            }

            if (time <= totalTime - 16 && OreRobots < Blueprint.OreForObsidianRobot * 2 && Ore >= Blueprint.OreForOreRobot)
            {
                var cp = Copy();
                cp.Ore -= Blueprint.OreForOreRobot;
                cp.Farm();
                cp.OreRobots++;
                mostGeodes = Math.Max(mostGeodes, cp.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode));
            }
            if (time <= totalTime - 8 && ClayRobots < Blueprint.ClayForObsidianRobot && Ore >= Blueprint.OreForClayRobot)
            {
                var cp = Copy();
                cp.Ore -= Blueprint.OreForClayRobot;
                cp.Farm();
                cp.ClayRobots++;
                mostGeodes = Math.Max(mostGeodes, cp.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode));
            }
            if (time <= totalTime - 4 && ObsidianRobots < Blueprint.ObsidianForGeodeRobot && Ore >= Blueprint.OreForObsidianRobot && Clay >= Blueprint.ClayForObsidianRobot)
            {
                var cp = Copy();
                cp.Ore -= Blueprint.OreForObsidianRobot;
                cp.Clay -= Blueprint.ClayForObsidianRobot;
                cp.Farm();
                cp.ObsidianRobots++;
                mostGeodes = Math.Max(mostGeodes, cp.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode));
            }

            var cpy = Copy();
            cpy.Farm();
            mostGeodes = Math.Max(mostGeodes, cpy.CalcMostGeodes(time + 1, memo, totalTime, earliestGeode));

            memo[h] = mostGeodes;
            return mostGeodes;
        }
    }

    public static List<Blueprint> ParseInput(string input)
    {
        var pattern = new Regex(@"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.");
        var blueprints = new List<Blueprint>();
        foreach (var line in input.Split('\n'))
        {
            var match = pattern.Match(line);
            if (match.Success)
            {
                int id = int.Parse(match.Groups[1].Value);
                int oreForOreRobot = int.Parse(match.Groups[2].Value);
                int oreForClayRobot = int.Parse(match.Groups[3].Value);
                int oreForObsidianRobot = int.Parse(match.Groups[4].Value);
                int clayForObsidianRobot = int.Parse(match.Groups[5].Value);
                int oreForGeodeRobot = int.Parse(match.Groups[6].Value);
                int obsidianForGeodeRobot = int.Parse(match.Groups[7].Value);
                blueprints.Add(new Blueprint(id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot));
            }
        }
        return blueprints;
    }
}
