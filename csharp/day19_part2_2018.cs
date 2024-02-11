
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Solution
{
    delegate int Operation(int[] r, int a, int b);

    static Dictionary<string, Operation> instructions = new Dictionary<string, Operation>
    {
        { "addr", (r, a, b) => r[a] + r[b] },
        { "addi", (r, a, b) => r[a] + b },
        { "mulr", (r, a, b) => r[a] * r[b] },
        { "muli", (r, a, b) => r[a] * b },
        { "banr", (r, a, b) => r[a] & r[b] },
        { "bani", (r, a, b) => r[a] & b },
        { "borr", (r, a, b) => r[a] | r[b] },
        { "bori", (r, a, b) => r[a] | b },
        { "setr", (r, a, b) => r[a] },
        { "seti", (r, a, b) => a },
        { "gtir", (r, a, b) => a > r[b] ? 1 : 0 },
        { "gtri", (r, a, b) => r[a] > b ? 1 : 0 },
        { "gtrr", (r, a, b) => r[a] > r[b] ? 1 : 0 },
        { "eqir", (r, a, b) => a == r[b] ? 1 : 0 },
        { "eqri", (r, a, b) => r[a] == b ? 1 : 0 },
        { "eqrr", (r, a, b) => r[a] == r[b] ? 1 : 0 }
    };

    static (int, List<Action<int[]>>) LoadProgram(string[] lines)
    {
        List<Action<int[]>> program = new List<Action<int[]>>();
        int ipRegister = 0;
        Regex regex = new Regex(@"\d+");

        foreach (string line in lines)
        {
            if (line.StartsWith("#ip"))
            {
                ipRegister = int.Parse(line.Split()[1]);
                continue;
            }

            string[] parts = line.Split();
            Operation op = instructions[parts[0]];
            MatchCollection matches = regex.Matches(line);
            int a = int.Parse(matches[0].Value);
            int b = int.Parse(matches[1].Value);
            int c = int.Parse(matches[2].Value);

            program.Add(r => r[c] = op(r, a, b));
        }

        return (ipRegister, program);
    }

    static int[] RunProgram(int ipRegister, List<Action<int[]>> program, int[] registers, int maxCycles)
    {
        int ip = 0;
        int cycles = 0;

        while (ip >= 0 && ip < program.Count)
        {
            registers[ipRegister] = ip;
            program[ip](registers);
            ip = registers[ipRegister] + 1;
            cycles++;
            if (maxCycles > 0 && cycles >= maxCycles)
            {
                break;
            }
        }

        return registers;
    }

    static int Max(int[] array)
    {
        int maxValue = array[0];
        foreach (int value in array)
        {
            if (value > maxValue)
            {
                maxValue = value;
            }
        }
        return maxValue;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        (int ipRegister, List<Action<int[]>> program) = LoadProgram(lines);

        int[] registers = new int[6];
        registers[0] = 1;
        registers = RunProgram(ipRegister, program, registers, 1000);
        int n = Max(registers);
        int total = 0;
        for (int i = 1; i <= n; i++)
        {
            if (n % i == 0)
            {
                total += i;
            }
        }
        Console.WriteLine(total);
    }
}
