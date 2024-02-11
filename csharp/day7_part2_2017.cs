
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

class Program
{
    class ProgramData
    {
        public int Weight { get; set; }
        public List<string> Holds { get; set; }

        public ProgramData()
        {
            Holds = new List<string>();
        }
    }

    static (int, bool) Dfs(string name, Dictionary<string, ProgramData> programs)
    {
        var program = programs[name];
        var totalWeight = program.Weight;

        var weights = new Dictionary<int, int>();
        foreach (var child in program.Holds)
        {
            var (weight, balanced) = Dfs(child, programs);
            if (!balanced)
            {
                return (0, false);
            }
            totalWeight += weight;
            if (weights.ContainsKey(weight))
            {
                weights[weight]++;
            }
            else
            {
                weights[weight] = 1;
            }
        }

        foreach (var w1 in weights.Keys)
        {
            foreach (var w2 in weights.Keys)
            {
                if (w1 != w2 && weights[w1] < weights[w2])
                {
                    string unbalancedProgram = "";
                    foreach (var child in program.Holds)
                    {
                        if (Dfs(child, programs).Item1 == w1)
                        {
                            unbalancedProgram = child;
                            break;
                        }
                    }
                    Console.WriteLine(programs[unbalancedProgram].Weight + (w2 - w1));
                    return (0, false);
                }
            }
        }

        return (totalWeight, true);
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        var programs = new Dictionary<string, ProgramData>();
        var re = new Regex("[a-z]+|\\d+");

        foreach (var line in lines)
        {
            var matches = re.Matches(line).Select(m => m.Value).ToArray();
            var name = matches[0];
            var weight = int.Parse(matches[1]);

            var program = new ProgramData { Weight = weight };
            if (matches.Length > 2)
            {
                program.Holds = matches.Skip(2).ToList();
            }
            programs[name] = program;
        }

        string root = "dtacyn"; // Replace this with the root found in Part One

        Dfs(root, programs);
    }
}
