
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
            { "b", 0 },
            { "c", 1 },
            { "d", 0 }
        };

        ExecuteInstructions(instructions, registers);

        Console.WriteLine(registers["a"]);
    }

    static void ExecuteInstructions(string[] instructions, Dictionary<string, int> registers)
    {
        for (int i = 0; i < instructions.Length;)
        {
            string[] parts = instructions[i].Split(' ');
            switch (parts[0])
            {
                case "cpy":
                    int val = GetValue(parts[1], registers);
                    registers[parts[2]] = val;
                    i++;
                    break;
                case "inc":
                    registers[parts[1]]++;
                    i++;
                    break;
                case "dec":
                    registers[parts[1]]--;
                    i++;
                    break;
                case "jnz":
                    int jump = GetValue(parts[1], registers);
                    if (jump != 0)
                    {
                        i += int.Parse(parts[2]);
                    }
                    else
                    {
                        i++;
                    }
                    break;
            }
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
}
