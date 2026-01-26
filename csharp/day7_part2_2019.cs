
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class AmplifierCircuit
{
    public static void Main()
    {
        try
        {
            string intcodeProgram = ReadInput("input.txt");
            Console.WriteLine("Part 1: " + FindMaxThrusterSignal(intcodeProgram, false));
            Console.WriteLine("Part 2: " + FindMaxThrusterSignal(intcodeProgram, true));
        }
        catch (Exception e)
        {
            Console.Error.WriteLine("Error reading input file: " + e.Message);
        }
    }

    private static string ReadInput(string filename)
    {
        return File.ReadAllText(filename).Trim();
    }

    private static long FindMaxThrusterSignal(string program, bool feedbackLoop)
    {
        long maxSignal = 0;
        int[] phaseSettings = feedbackLoop ? new int[] { 5, 6, 7, 8, 9 } : new int[] { 0, 1, 2, 3, 4 };
        var permutations = GeneratePermutations(phaseSettings);

        foreach (var permutation in permutations)
        {
            maxSignal = Math.Max(maxSignal, RunAmplifiers(program, permutation, feedbackLoop));
        }

        return maxSignal;
    }

    private static long RunAmplifiers(string program, int[] phaseSettings, bool feedbackLoop)
    {
        int numAmplifiers = phaseSettings.Length;
        var amplifiers = new IntcodeComputer[numAmplifiers];
        for (int i = 0; i < numAmplifiers; i++)
        {
            amplifiers[i] = new IntcodeComputer(program);
        }

        long signal = 0;
        bool firstRound = true;
        int ampIndex = 0;

        while (feedbackLoop ? !amplifiers[numAmplifiers - 1].IsHalted : ampIndex < numAmplifiers)
        {
            var currentAmp = amplifiers[ampIndex % numAmplifiers];

            if (firstRound)
            {
                currentAmp.AddInput(phaseSettings[ampIndex % numAmplifiers]);
            }
            currentAmp.AddInput(signal);

            signal = currentAmp.Run();
            ampIndex++;
            if (ampIndex >= numAmplifiers)
            {
                firstRound = false;
            }
        }

        return signal;
    }

    private static IEnumerable<int[]> GeneratePermutations(int[] array)
    {
        return Permute(array, 0, array.Length - 1);
    }

    private static IEnumerable<int[]> Permute(int[] array, int start, int end)
    {
        if (start == end)
        {
            yield return (int[])array.Clone();
        }
        else
        {
            for (int i = start; i <= end; i++)
            {
                Swap(ref array[start], ref array[i]);
                foreach (var p in Permute(array, start + 1, end))
                {
                    yield return p;
                }
                Swap(ref array[start], ref array[i]);
            }
        }
    }

    private static void Swap(ref int a, ref int b)
    {
        int temp = a;
        a = b;
        b = temp;
    }
}

public class IntcodeComputer
{
    private long[] memory;
    private int ip;
    private int relativeBase;
    private Queue<long> inputQueue;
    private long output;
    private bool halted;

    public IntcodeComputer(string program)
    {
        var opcodes = program.Split(',').Select(long.Parse).ToArray();
        this.memory = new long[10000];
        Array.Copy(opcodes, this.memory, opcodes.Length);
        this.ip = 0;
        this.relativeBase = 0;
        this.inputQueue = new Queue<long>();
        this.output = 0;
        this.halted = false;
    }

    public void AddInput(long input)
    {
        inputQueue.Enqueue(input);
    }

    public long Run()
    {
        while (true)
        {
            int instruction = (int)memory[ip];
            int opcode = instruction % 100;

            if (opcode == 99)
            {
                halted = true;
                return output;
            }

            int[] modes = new int[3];
            modes[0] = (instruction / 100) % 10;
            modes[1] = (instruction / 1000) % 10;
            modes[2] = (instruction / 10000) % 10;

            switch (opcode)
            {
                case 1: // Add
                    memory[GetAddress(ip + 3, modes[2])] = ReadValue(ip + 1, modes[0]) + ReadValue(ip + 2, modes[1]);
                    ip += 4;
                    break;
                case 2: // Multiply
                    memory[GetAddress(ip + 3, modes[2])] = ReadValue(ip + 1, modes[0]) * ReadValue(ip + 2, modes[1]);
                    ip += 4;
                    break;
                case 3: // Input
                    if (inputQueue.Count == 0)
                    {
                        return output; // Wait for input
                    }
                    memory[GetAddress(ip + 1, modes[0])] = inputQueue.Dequeue();
                    ip += 2;
                    break;
                case 4: // Output
                    output = ReadValue(ip + 1, modes[0]);
                    ip += 2;
                    return output;
                case 5: // Jump-if-true
                    if (ReadValue(ip + 1, modes[0]) != 0)
                    {
                        ip = (int)ReadValue(ip + 2, modes[1]);
                    }
                    else
                    {
                        ip += 3;
                    }
                    break;
                case 6: // Jump-if-false
                    if (ReadValue(ip + 1, modes[0]) == 0)
                    {
                        ip = (int)ReadValue(ip + 2, modes[1]);
                    }
                    else
                    {
                        ip += 3;
                    }
                    break;
                case 7: // Less than
                    memory[GetAddress(ip + 3, modes[2])] = ReadValue(ip + 1, modes[0]) < ReadValue(ip + 2, modes[1]) ? 1 : 0;
                    ip += 4;
                    break;
                case 8: // Equals
                    memory[GetAddress(ip + 3, modes[2])] = ReadValue(ip + 1, modes[0]) == ReadValue(ip + 2, modes[1]) ? 1 : 0;
                    ip += 4;
                    break;
                case 9: // Adjust relative base
                    relativeBase += (int)ReadValue(ip + 1, modes[0]);
                    ip += 2;
                    break;
                default:
                    throw new Exception("Invalid opcode: " + opcode);
            }
        }
    }

    private long ReadValue(int address, int mode)
    {
        return memory[GetAddress(address, mode)];
    }

    private int GetAddress(int address, int mode)
    {
        if (mode == 0)
        {
            return (int)memory[address];
        }
        else if (mode == 1)
        {
            return address;
        }
        else
        {
            return (int)memory[address] + relativeBase;
        }
    }

    public bool IsHalted => halted;
}
