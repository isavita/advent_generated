
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] instructions = File.ReadAllLines("input.txt");
        var result = ExecuteBootCode(instructions);
        Console.WriteLine(result);
    }

    static Tuple<int, bool> ExecuteBootCode(string[] instructions)
    {
        int accumulator = 0;
        var visited = new HashSet<int>();
        int currentInstruction = 0;

        while (currentInstruction < instructions.Length)
        {
            if (visited.Contains(currentInstruction))
            {
                return Tuple.Create(accumulator, true);
            }

            visited.Add(currentInstruction);
            string[] parts = instructions[currentInstruction].Split(' ');
            string op = parts[0];
            int arg = int.Parse(parts[1]);

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

        return Tuple.Create(accumulator, false);
    }
}
