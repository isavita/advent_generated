
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] instructions = File.ReadAllLines("input.txt");

        Dictionary<string, int> registers = new Dictionary<string, int>
        {
            { "a", 0 },
            { "b", 0 }
        };

        for (int i = 0; i < instructions.Length; i++)
        {
            string[] parts = instructions[i].Split(' ');

            switch (parts[0])
            {
                case "hlf":
                    registers[parts[1]] /= 2;
                    break;
                case "tpl":
                    registers[parts[1]] *= 3;
                    break;
                case "inc":
                    registers[parts[1]]++;
                    break;
                case "jmp":
                    i += int.Parse(parts[1]) - 1;
                    break;
                case "jie":
                    if (registers[parts[1].Substring(0, 1)] % 2 == 0)
                    {
                        i += int.Parse(parts[2]) - 1;
                    }
                    break;
                case "jio":
                    if (registers[parts[1].Substring(0, 1)] == 1)
                    {
                        i += int.Parse(parts[2]) - 1;
                    }
                    break;
                default:
                    throw new Exception($"Unknown instruction: {parts[0]}");
            }
        }

        Console.WriteLine(registers["b"]);
    }
}
