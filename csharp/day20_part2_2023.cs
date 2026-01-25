
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public enum ModuleType
{
    Broadcaster,
    FlipFlop,
    Conjunction
}

public class Module
{
    public string Name { get; set; }
    public ModuleType Type { get; set; }
    public List<int> OutputIndices { get; set; }
    public List<string> OutputNames { get; set; }
    public bool FlipFlopState { get; set; }
    public List<int> InputIndices { get; set; }
    public List<bool> InputLastPulse { get; set; }

    public Module()
    {
        OutputIndices = new List<int>();
        OutputNames = new List<string>();
        InputIndices = new List<int>();
        InputLastPulse = new List<bool>();
    }
}

public class PulseState
{
    public int FromModuleIndex { get; set; }
    public int ToModuleIndex { get; set; }
    public bool Pulse { get; set; }
}

public class Program
{
    private static List<Module> modules = new List<Module>();
    private static Queue<PulseState> queue = new Queue<PulseState>();

    public static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        foreach (string line in lines)
        {
            if (!string.IsNullOrEmpty(line))
            {
                AddModuleDefinition(line);
            }
        }

        ResolveConnections();

        int broadcasterIndex = modules.FindIndex(m => m.Name == "broadcaster");
        if (broadcasterIndex == -1)
        {
            Console.Error.WriteLine("Error: Broadcaster module not found");
            return;
        }

        int rxFeederIndex = -1;
        for (int i = 0; i < modules.Count; i++)
        {
            if (modules[i].OutputNames.Contains("rx"))
            {
                rxFeederIndex = i;
                break;
            }
        }

        if (rxFeederIndex == -1 || modules[rxFeederIndex].Type != ModuleType.Conjunction)
        {
            Console.Error.WriteLine("Error: Expected a single conjunction module feeding 'rx'. Structure not supported.");
            return;
        }

        Module feederConjunction = modules[rxFeederIndex];
        long[] loopLengths = new long[feederConjunction.InputIndices.Count];
        int loopsFound = 0;

        long pressCount = 0;
        while (loopsFound < feederConjunction.InputIndices.Count)
        {
            pressCount++;
            Enqueue(-1, broadcasterIndex, false);

            while (queue.Count > 0)
            {
                PulseState currentState = Dequeue();
                int fromIndex = currentState.FromModuleIndex;
                int toIndex = currentState.ToModuleIndex;
                bool pulse = currentState.Pulse;

                if (toIndex == rxFeederIndex && pulse)
                {
                    for (int i = 0; i < feederConjunction.InputIndices.Count; i++)
                    {
                        if (feederConjunction.InputIndices[i] == fromIndex && loopLengths[i] == 0)
                        {
                            loopLengths[i] = pressCount;
                            loopsFound++;
                        }
                    }
                }

                if (toIndex == -1) continue;

                Module targetModule = modules[toIndex];

                switch (targetModule.Type)
                {
                    case ModuleType.FlipFlop:
                        if (!pulse)
                        {
                            targetModule.FlipFlopState = !targetModule.FlipFlopState;
                            bool pulseToSend = targetModule.FlipFlopState;
                            foreach (int outputIndex in targetModule.OutputIndices)
                            {
                                Enqueue(toIndex, outputIndex, pulseToSend);
                            }
                        }
                        break;
                    case ModuleType.Conjunction:
                        for (int i = 0; i < targetModule.InputIndices.Count; i++)
                        {
                            if (targetModule.InputIndices[i] == fromIndex)
                            {
                                targetModule.InputLastPulse[i] = pulse;
                            }
                        }
                        bool allHigh = targetModule.InputLastPulse.All(p => p);
                        bool pulseToSendConjunction = !allHigh;
                        foreach (int outputIndex in targetModule.OutputIndices)
                        {
                            Enqueue(toIndex, outputIndex, pulseToSendConjunction);
                        }
                        break;
                    case ModuleType.Broadcaster:
                        bool pulseToSendBroadcaster = pulse;
                        foreach (int outputIndex in targetModule.OutputIndices)
                        {
                            Enqueue(toIndex, outputIndex, pulseToSendBroadcaster);
                        }
                        break;
                }
            }

            if (pressCount > 1000000)
            {
                Console.Error.WriteLine("Warning: Exceeded 1,000,000 presses, may be infinite loop or very long cycle.");
                break;
            }
        }

        long finalLcm = 1;
        if (loopsFound == feederConjunction.InputIndices.Count)
        {
            foreach (long loopLength in loopLengths)
            {
                finalLcm = LCM(finalLcm, loopLength);
            }
        }
        else
        {
            Console.Error.WriteLine("Error: Could not find all required loop lengths.");
            finalLcm = 0;
        }

        Console.WriteLine(finalLcm);
    }

    private static void AddModuleDefinition(string line)
    {
        Module module = new Module();
        string[] parts = line.Split(new[] { " -> " }, StringSplitOptions.None);
        string moduleName = parts[0].Trim();

        if (moduleName.StartsWith("%"))
        {
            module.Type = ModuleType.FlipFlop;
            module.Name = moduleName.Substring(1);
        }
        else if (moduleName.StartsWith("&"))
        {
            module.Type = ModuleType.Conjunction;
            module.Name = moduleName.Substring(1);
        }
        else
        {
            module.Type = ModuleType.Broadcaster;
            module.Name = moduleName;
        }

        string[] outputNames = parts[1].Split(new[] { ", " }, StringSplitOptions.None);
        module.OutputNames.AddRange(outputNames);

        modules.Add(module);
    }

    private static void ResolveConnections()
    {
        for (int i = 0; i < modules.Count; i++)
        {
            Module module = modules[i];
            foreach (string outputName in module.OutputNames)
            {
                int outputIndex = modules.FindIndex(m => m.Name == outputName);
                if (outputIndex != -1)
                {
                    module.OutputIndices.Add(outputIndex);
                    Module targetModule = modules[outputIndex];
                    if (targetModule.Type == ModuleType.Conjunction)
                    {
                        targetModule.InputIndices.Add(i);
                        targetModule.InputLastPulse.Add(false);
                    }
                }
            }
        }
    }

    private static void Enqueue(int fromModuleIndex, int toModuleIndex, bool pulse)
    {
        queue.Enqueue(new PulseState
        {
            FromModuleIndex = fromModuleIndex,
            ToModuleIndex = toModuleIndex,
            Pulse = pulse
        });
    }

    private static PulseState Dequeue()
    {
        return queue.Dequeue();
    }

    private static long GCD(long a, long b)
    {
        while (b != 0)
        {
            long temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    private static long LCM(long a, long b)
    {
        return a * b / GCD(a, b);
    }
}
