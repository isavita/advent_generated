
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        string[] instructions = LoadInstructionsFromFile("input.txt");
        Dictionary<string, int> registers = new Dictionary<string, int> { { "a", 0 }, { "b", 0 }, { "c", 0 }, { "d", 0 } };
        ExecuteInstructions(instructions, registers);
        Console.WriteLine(registers["a"]);
    }

    static string[] LoadInstructionsFromFile(string fileName)
    {
        try
        {
            return File.ReadAllLines(fileName);
        }
        catch (Exception ex)
        {
            Console.WriteLine("An error occurred while reading the file: " + ex.Message);
            return new string[0]; // Return an empty array if file reading fails
        }
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
                        int.TryParse(parts[2], out int result);
                        i += result;
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
