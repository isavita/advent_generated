
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] instructions = File.ReadAllLines("input.txt");
        
        for (int a = 1; ; a++)
        {
            if (ProducesClockSignal(a, instructions))
            {
                Console.WriteLine(a);
                break;
            }
        }
    }

    static bool ProducesClockSignal(int a, string[] instructions)
    {
        Dictionary<string, int> registers = new Dictionary<string, int>
        {
            { "a", a },
            { "b", 0 },
            { "c", 0 },
            { "d", 0 }
        };
        int outputCount = 0;
        int expectedOutput = 0;

        for (int i = 0; i < instructions.Length; i++)
        {
            string[] parts = instructions[i].Split(' ');
            switch (parts[0])
            {
                case "cpy":
                    int val = GetValue(parts[1], registers);
                    registers[parts[2]] = val;
                    break;
                case "inc":
                    registers[parts[1]]++;
                    break;
                case "dec":
                    registers[parts[1]]--;
                    break;
                case "jnz":
                    int checkVal = GetValue(parts[1], registers);
                    int jump = GetValue(parts[2], registers);
                    if (checkVal != 0)
                    {
                        i += jump - 1;
                    }
                    break;
                case "out":
                    int output = GetValue(parts[1], registers);
                    if (output != expectedOutput)
                    {
                        return false;
                    }
                    expectedOutput = 1 - expectedOutput;
                    outputCount++;
                    if (outputCount >= 100)
                    {
                        return true;
                    }
                    break;
            }
        }
        return false;
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
