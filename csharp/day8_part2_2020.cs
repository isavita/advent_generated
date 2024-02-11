
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] instructions = File.ReadAllLines("input.txt");

        for (int i = 0; i < instructions.Length; i++)
        {
            (string op, int arg) = ParseInstruction(instructions[i]);
            if (op == "acc")
            {
                continue;
            }

            string[] modifiedInstructions = instructions.ToArray();
            if (op == "jmp")
            {
                modifiedInstructions[i] = $"nop {arg}";
            }
            else
            {
                modifiedInstructions[i] = $"jmp {arg}";
            }

            (int accumulator, bool terminated) = ExecuteBootCode(modifiedInstructions);
            if (terminated)
            {
                Console.WriteLine(accumulator);
                break;
            }
        }
    }

    static (int, bool) ExecuteBootCode(string[] instructions)
    {
        int accumulator = 0;
        bool[] visited = new bool[instructions.Length];
        int currentInstruction = 0;

        while (currentInstruction < instructions.Length)
        {
            if (visited[currentInstruction])
            {
                return (accumulator, false);
            }

            visited[currentInstruction] = true;
            (string op, int arg) = ParseInstruction(instructions[currentInstruction]);

            switch (op)
            {
                case "acc":
                    accumulator += arg;
                    currentInstruction++;
                    break;
                case "jmp":
                    currentInstruction += arg;
                    break;
                case "nop":
                    currentInstruction++;
                    break;
            }
        }

        return (accumulator, true);
    }

    static (string, int) ParseInstruction(string instruction)
    {
        string[] parts = instruction.Split(' ');
        string op = parts[0];
        int arg = int.Parse(parts[1]);
        return (op, arg);
    }
}
