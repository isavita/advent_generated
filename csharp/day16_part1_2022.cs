
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public class Valve
{
    public string Id { get; set; }
    public int Flow { get; set; }
    public Dictionary<string, int> Tunnels { get; set; } = new Dictionary<string, int>();
}

public class Program
{
    public static void Main(string[] args)
    {
        var input = File.ReadAllText("input.txt").Trim();
        var valves = ParseInput(input);

        PrecomputeDistances(valves);

        var openValves = valves.Values.Where(v => v.Flow > 0).Select(v => v.Id).ToList();

        Console.WriteLine(MaxPressure(valves, "AA", 30, 0, openValves));
    }

    private static Dictionary<string, Valve> ParseInput(string input)
    {
        var valves = new Dictionary<string, Valve>();
        var pattern = new Regex(@"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)");

        foreach (var line in input.Split('\n'))
        {
            var match = pattern.Match(line);
            if (match.Success)
            {
                var v = new Valve
                {
                    Id = match.Groups[1].Value,
                    Flow = int.Parse(match.Groups[2].Value),
                };
                v.Tunnels[v.Id] = 0;
                foreach (var tunnel in match.Groups[3].Value.Split(", "))
                {
                    v.Tunnels[tunnel] = 1;
                }
                valves[v.Id] = v;
            }
        }

        return valves;
    }

    private static void PrecomputeDistances(Dictionary<string, Valve> valves)
    {
        var ids = valves.Keys.ToList();
        foreach (var k in ids)
        {
            foreach (var i in ids)
            {
                foreach (var j in ids)
                {
                    if (valves[i].Tunnels.ContainsKey(k) && valves[k].Tunnels.ContainsKey(j))
                    {
                        if (!valves[i].Tunnels.ContainsKey(j) || valves[i].Tunnels[j] > valves[i].Tunnels[k] + valves[k].Tunnels[j])
                        {
                            valves[i].Tunnels[j] = valves[i].Tunnels[k] + valves[k].Tunnels[j];
                        }
                    }
                }
            }
        }
    }

    private static int MaxPressure(Dictionary<string, Valve> valves, string curr, int minute, int pressure, List<string> openValves, int depth = 0)
    {
        var max = pressure;
        foreach (var next in openValves)
        {
            var newOpenValves = new List<string>(openValves);
            newOpenValves.Remove(next);
            var timeLeft = minute - valves[curr].Tunnels[next] - 1;
            if (timeLeft > 0)
            {
                max = Math.Max(max, MaxPressure(valves, next, timeLeft, timeLeft * valves[next].Flow + pressure, newOpenValves, depth + 1));
            }
        }
        return max;
    }
}
