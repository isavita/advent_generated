
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        string[] instructions = File.ReadAllLines("input.txt");
        Dictionary<string, int> registers = new Dictionary<string, int> { { "a", 7 }, { "b", 0 }, { "c", 0 }, { "d", 0 } };
        ExecuteInstructions(instructions, registers);
        Console.WriteLine(registers["a"]);
    }

    static void ExecuteInstructions(string[] instructions, Dictionary<string, int> registers)
    {
        int pc = 0;
        while (pc < instructions.Length)
        {
            string[] fields = instructions[pc].Split(' ');
            switch (fields[0])
            {
                case "cpy":
                    int x = GetValue(fields[1], registers);
                    if (registers.ContainsKey(fields[2]))
                    {
                        registers[fields[2]] = x;
                    }
                    break;
                case "inc":
                    if (registers.ContainsKey(fields[1]))
                    {
                        registers[fields[1]]++;
                    }
                    break;
                case "dec":
                    if (registers.ContainsKey(fields[1]))
                    {
                        registers[fields[1]]--;
                    }
                    break;
                case "jnz":
                    int val = GetValue(fields[1], registers);
                    if (val != 0)
                    {
                        pc += GetValue(fields[2], registers) - 1;
                    }
                    break;
                case "tgl":
                    int target = pc + GetValue(fields[1], registers);
                    if (target >= 0 && target < instructions.Length)
                    {
                        instructions[target] = ToggleInstruction(instructions[target]);
                    }
                    break;
            }
            pc++;
        }
    }

    static int GetValue(string s, Dictionary<string, int> registers)
    {
        if (int.TryParse(s, out int val))
        {
            return val;
        }
        return registers[s];
    }

    static string ToggleInstruction(string instr)
    {
        string[] parts = instr.Split(' ');
        switch (parts[0])
        {
            case "inc":
                parts[0] = "dec";
                break;
            case "dec":
            case "tgl":
                parts[0] = "inc";
                break;
            case "jnz":
                parts[0] = "cpy";
                break;
            case "cpy":
                parts[0] = "jnz";
                break;
        }
        return string.Join(" ", parts);
    }
}
