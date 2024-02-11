
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Module
{
    public string name;
    public char prefix;
    public List<string> destinations;
    public bool state;
    public Dictionary<string, PulseValue> memory;
}

public enum PulseValue
{
    Low,
    High
}

public class Pulse
{
    public PulseValue value;
    public string fromName;
    public string toName;
}

public class Solution
{
    private const char FlipFlop = '%';
    private const char Conjunction = '&';

    private static Dictionary<string, Module> ParseInput(string[] input)
    {
        char[] prefixes = { FlipFlop, Conjunction };
        Dictionary<string, Module> modules = new Dictionary<string, Module>();

        foreach (string line in input)
        {
            string[] parts = line.Split(" -> ");

            Module module = new Module();
            bool isPrefix = false;
            foreach (char prefix in prefixes)
            {
                if (parts[0][0] == prefix)
                {
                    module.prefix = prefix;
                    module.name = parts[0].Substring(1);
                    isPrefix = true;
                    break;
                }
            }
            if (!isPrefix)
            {
                module.name = parts[0];
            }
            module.destinations = parts[1].Split(", ").ToList();
            module.memory = new Dictionary<string, PulseValue>();

            modules[module.name] = module;
        }

        foreach (Module module in modules.Values)
        {
            foreach (string destName in module.destinations)
            {
                if (modules.TryGetValue(destName, out Module destModule) && destModule.prefix == Conjunction)
                {
                    destModule.memory[module.name] = PulseValue.Low;
                }
            }
        }

        return modules;
    }

    private static Tuple<int, int> PushButton(Dictionary<string, Module> modules, Pulse startPulse, int numCycle)
    {
        int cntLow = 0;
        int cntHigh = 0;
        List<Pulse> pulseQueue = new List<Pulse>();

        for (int i = 0; i < numCycle; i++)
        {
            pulseQueue.Add(startPulse);

            while (pulseQueue.Count > 0)
            {
                Pulse pulse = pulseQueue[0];
                pulseQueue.RemoveAt(0);

                if (pulse.value == PulseValue.Low)
                {
                    cntLow++;
                }
                else
                {
                    cntHigh++;
                }

                if (!modules.ContainsKey(pulse.toName))
                {
                    continue;
                }

                Module module = modules[pulse.toName];
                PulseValue newPulseValue;
                switch (module.prefix)
                {
                    case FlipFlop:
                        if (pulse.value == PulseValue.Low)
                        {
                            module.state = !module.state;
                            newPulseValue = module.state ? PulseValue.High : PulseValue.Low;
                        }
                        else
                        {
                            continue;
                        }
                        break;

                    case Conjunction:
                        module.memory[pulse.fromName] = pulse.value;
                        bool isHighForAll = module.memory.All(pair => pair.Value == PulseValue.High);

                        newPulseValue = isHighForAll ? PulseValue.Low : PulseValue.High;
                        break;

                    default:
                        newPulseValue = pulse.value;
                        break;
                }

                foreach (string destName in module.destinations)
                {
                    Pulse newPulse = new Pulse
                    {
                        value = newPulseValue,
                        fromName = pulse.toName,
                        toName = destName
                    };
                    pulseQueue.Add(newPulse);
                }
            }
        }

        return new Tuple<int, int>(cntLow, cntHigh);
    }

    private static int Solve(string[] input)
    {
        Pulse startPulse = new Pulse
        {
            value = PulseValue.Low,
            fromName = "button",
            toName = "broadcaster"
        };
        int numCycle = 1000;

        Dictionary<string, Module> modules = ParseInput(input);

        Tuple<int, int> result = PushButton(modules, startPulse, numCycle);

        return result.Item1 * result.Item2;
    }

    private static string[] ReadFile(string fileName)
    {
        return File.ReadAllLines(fileName);
    }

    public static void Main()
    {
        string[] input = ReadFile("input.txt");
        Console.WriteLine(Solve(input));
    }
}
