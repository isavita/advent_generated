
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] instructions = File.ReadAllLines("input.txt");
        string code = GetBathroomCode(instructions);
        Console.WriteLine(code);
    }

    static string GetBathroomCode(string[] instructions)
    {
        Dictionary<string, Dictionary<char, string>> keypad = new Dictionary<string, Dictionary<char, string>>()
        {
            { "1", new Dictionary<char, string> { { 'D', "3" } } },
            { "2", new Dictionary<char, string> { { 'R', "3" }, { 'D', "6" } } },
            { "3", new Dictionary<char, string> { { 'U', "1" }, { 'R', "4" }, { 'D', "7" }, { 'L', "2" } } },
            { "4", new Dictionary<char, string> { { 'L', "3" }, { 'D', "8" } } },
            { "5", new Dictionary<char, string> { { 'R', "6" } } },
            { "6", new Dictionary<char, string> { { 'U', "2" }, { 'R', "7" }, { 'D', "A" }, { 'L', "5" } } },
            { "7", new Dictionary<char, string> { { 'U', "3" }, { 'R', "8" }, { 'D', "B" }, { 'L', "6" } } },
            { "8", new Dictionary<char, string> { { 'U', "4" }, { 'R', "9" }, { 'D', "C" }, { 'L', "7" } } },
            { "9", new Dictionary<char, string> { { 'L', "8" } } },
            { "A", new Dictionary<char, string> { { 'U', "6" }, { 'R', "B" } } },
            { "B", new Dictionary<char, string> { { 'U', "7" }, { 'R', "C" }, { 'D', "D" }, { 'L', "A" } } },
            { "C", new Dictionary<char, string> { { 'U', "8" }, { 'L', "B" } } },
            { "D", new Dictionary<char, string> { { 'U', "B" } } }
        };

        string position = "5";
        string code = "";

        foreach (string instruction in instructions)
        {
            foreach (char move in instruction)
            {
                if (keypad[position].ContainsKey(move))
                {
                    position = keypad[position][move];
                }
            }
            code += position;
        }

        return code;
    }
}
