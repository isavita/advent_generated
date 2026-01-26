
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string input = File.ReadAllText("input.txt");
        Console.WriteLine(Solve(input));
    }

    static int Solve(string input)
    {
        var opcodeComputer = ParseInput(input);
        int lastReg5 = 0;
        var comparedRegister5s = new HashSet<int>();
        while (true)
        {
            if (opcodeComputer.Registers[opcodeComputer.InstructionPointer] == 28)
            {
                int reg5 = opcodeComputer.Registers[5];
                if (comparedRegister5s.Contains(reg5))
                {
                    break;
                }
                comparedRegister5s.Add(reg5);
                lastReg5 = reg5;
            }
            if (opcodeComputer.Tick()) break;
        }
        return lastReg5;
    }

    public class OpcodeComputer
    {
        public Instruction[] Instructions { get; set; }
        public int[] Registers { get; set; } = new int[6];
        public int InstructionPointer { get; set; }

        public bool Tick()
        {
            if (Registers[InstructionPointer] >= Instructions.Length)
            {
                return true;
            }
            var inst = Instructions[Registers[InstructionPointer]];
            Registers = inst.OpcodeFunc(Registers, inst.AbcValues);
            Registers[InstructionPointer]++;
            return Registers[InstructionPointer] >= Instructions.Length;
        }
    }

    public class Instruction
    {
        public Func<int[], int[], int[]> OpcodeFunc { get; set; }
        public int[] AbcValues { get; set; } = new int[3];
    }

    static OpcodeComputer ParseInput(string input)
    {
        var lines = input.Split('\n');
        var ipPattern = new Regex("#ip (\\d+)");
        var ipMatch = ipPattern.Match(lines[0]);
        int instructionPointer = int.Parse(ipMatch.Groups[1].Value);

        var instructionsList = new List<Instruction>();
        var instructionPattern = new Regex("(\\w+) (\\d+) (\\d+) (\\d+)");
        for (int i = 1; i < lines.Length; i++)
        {
            var match = instructionPattern.Match(lines[i]);
            if (match.Success)
            {
                var inst = new Instruction
                {
                    OpcodeFunc = OpcodeNamesToFuncs[match.Groups[1].Value],
                    AbcValues = new int[]
                    {
                        int.Parse(match.Groups[2].Value),
                        int.Parse(match.Groups[3].Value),
                        int.Parse(match.Groups[4].Value)
                    }
                };
                instructionsList.Add(inst);
            }
        }

        return new OpcodeComputer
        {
            Instructions = instructionsList.ToArray(),
            InstructionPointer = instructionPointer
        };
    }

    static readonly Dictionary<string, Func<int[], int[], int[]>> OpcodeNamesToFuncs = new Dictionary<string, Func<int[], int[], int[]>>
    {
        ["addr"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] + registers[abcValues[1]]; return registers; },
        ["addi"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] + abcValues[1]; return registers; },
        ["mulr"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] * registers[abcValues[1]]; return registers; },
        ["muli"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] * abcValues[1]; return registers; },
        ["banr"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] & registers[abcValues[1]]; return registers; },
        ["bani"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] & abcValues[1]; return registers; },
        ["borr"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] | registers[abcValues[1]]; return registers; },
        ["bori"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] | abcValues[1]; return registers; },
        ["setr"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]]; return registers; },
        ["seti"] = (registers, abcValues) => { registers[abcValues[2]] = abcValues[0]; return registers; },
        ["gtir"] = (registers, abcValues) => { registers[abcValues[2]] = abcValues[0] > registers[abcValues[1]] ? 1 : 0; return registers; },
        ["gtri"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] > abcValues[1] ? 1 : 0; return registers; },
        ["gtrr"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] > registers[abcValues[1]] ? 1 : 0; return registers; },
        ["eqir"] = (registers, abcValues) => { registers[abcValues[2]] = abcValues[0] == registers[abcValues[1]] ? 1 : 0; return registers; },
        ["eqri"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] == abcValues[1] ? 1 : 0; return registers; },
        ["eqrr"] = (registers, abcValues) => { registers[abcValues[2]] = registers[abcValues[0]] == registers[abcValues[1]] ? 1 : 0; return registers; }
    };
}
