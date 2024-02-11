
using System;
using System.IO;
using System.Linq;

class Solution
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int ipBind = int.Parse(lines[0].Split()[1]);
        string[][] instructions = lines.Skip(1).Select(line => line.Split()).ToArray();

        int[] registers = new int[6];
        for (int ip = 0; ip < instructions.Length; ip++)
        {
            registers[ipBind] = ip;
            string[] inst = instructions[ip];
            string opcode = inst[0];
            int a = int.Parse(inst[1]);
            int b = int.Parse(inst[2]);
            int c = int.Parse(inst[3]);

            switch (opcode)
            {
                case "addr":
                    registers[c] = registers[a] + registers[b];
                    break;
                case "addi":
                    registers[c] = registers[a] + b;
                    break;
                case "mulr":
                    registers[c] = registers[a] * registers[b];
                    break;
                case "muli":
                    registers[c] = registers[a] * b;
                    break;
                case "banr":
                    registers[c] = registers[a] & registers[b];
                    break;
                case "bani":
                    registers[c] = registers[a] & b;
                    break;
                case "borr":
                    registers[c] = registers[a] | registers[b];
                    break;
                case "bori":
                    registers[c] = registers[a] | b;
                    break;
                case "setr":
                    registers[c] = registers[a];
                    break;
                case "seti":
                    registers[c] = a;
                    break;
                case "gtir":
                    registers[c] = a > registers[b] ? 1 : 0;
                    break;
                case "gtri":
                    registers[c] = registers[a] > b ? 1 : 0;
                    break;
                case "gtrr":
                    registers[c] = registers[a] > registers[b] ? 1 : 0;
                    break;
                case "eqir":
                    registers[c] = a == registers[b] ? 1 : 0;
                    break;
                case "eqri":
                    registers[c] = registers[a] == b ? 1 : 0;
                    break;
                case "eqrr":
                    registers[c] = registers[a] == registers[b] ? 1 : 0;
                    break;
            }

            ip = registers[ipBind];
            if (ip < 0 || ip >= instructions.Length)
            {
                break;
            }
        }

        Console.WriteLine(registers[0]);
    }
}
