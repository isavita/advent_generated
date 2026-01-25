
using System;
using System.Collections.Generic;
using System.IO;

class Instruction
{
    public string Name;
    public int[] Abc = new int[3];
}

class OpcodeComputer
{
    public List<Instruction> Instructions = new List<Instruction>();
    public int[] Registers = new int[6];
    public int IpRegister;
}

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var computer = new OpcodeComputer();
        computer.IpRegister = int.Parse(lines[0].Split(' ')[1]);
        for (int i = 1; i < lines.Length; i++)
        {
            var parts = lines[i].Split(' ');
            var instr = new Instruction { Name = parts[0] };
            instr.Abc[0] = int.Parse(parts[1]);
            instr.Abc[1] = int.Parse(parts[2]);
            instr.Abc[2] = int.Parse(parts[3]);
            computer.Instructions.Add(instr);
        }

        while (true)
        {
            int ip = computer.Registers[computer.IpRegister];
            if (ip < 0 || ip >= computer.Instructions.Count) break;

            var ins = computer.Instructions[ip];
            var a = ins.Abc[0];
            var b = ins.Abc[1];
            var c = ins.Abc[2];
            switch (ins.Name)
            {
                case "addr": computer.Registers[c] = computer.Registers[a] + computer.Registers[b]; break;
                case "addi": computer.Registers[c] = computer.Registers[a] + b; break;
                case "mulr": computer.Registers[c] = computer.Registers[a] * computer.Registers[b]; break;
                case "muli": computer.Registers[c] = computer.Registers[a] * b; break;
                case "banr": computer.Registers[c] = computer.Registers[a] & computer.Registers[b]; break;
                case "bani": computer.Registers[c] = computer.Registers[a] & b; break;
                case "borr": computer.Registers[c] = computer.Registers[a] | computer.Registers[b]; break;
                case "bori": computer.Registers[c] = computer.Registers[a] | b; break;
                case "setr": computer.Registers[c] = computer.Registers[a]; break;
                case "seti": computer.Registers[c] = a; break;
                case "gtir": computer.Registers[c] = a > computer.Registers[b] ? 1 : 0; break;
                case "gtri": computer.Registers[c] = computer.Registers[a] > b ? 1 : 0; break;
                case "gtrr": computer.Registers[c] = computer.Registers[a] > computer.Registers[b] ? 1 : 0; break;
                case "eqir": computer.Registers[c] = a == computer.Registers[b] ? 1 : 0; break;
                case "eqri": computer.Registers[c] = computer.Registers[a] == b ? 1 : 0; break;
                case "eqrr": computer.Registers[c] = computer.Registers[a] == computer.Registers[b] ? 1 : 0; break;
            }

            computer.Registers[computer.IpRegister]++;

            if (computer.Registers[computer.IpRegister] == 28) break;
        }

        Console.WriteLine(computer.Registers[5]);
    }
}
