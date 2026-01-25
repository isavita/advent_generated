
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static Dictionary<string, string> rules = new Dictionary<string, string>();
    static Dictionary<string, ushort> memo = new Dictionary<string, ushort>();

    static ushort Eval(string key)
    {
        if (memo.TryGetValue(key, out ushort cached)) return cached;

        ushort value;
        if (ushort.TryParse(key, out value))
            return value;

        string rule = rules[key];
        string[] parts = rule.Split(' ');

        if (parts.Length == 1)
            value = Eval(parts[0]);
        else if (parts[0] == "NOT")
            value = (ushort)~Eval(parts[1]);
        else
        {
            ushort left = Eval(parts[0]);
            ushort right = Eval(parts[2]);
            switch (parts[1])
            {
                case "AND": value = (ushort)(left & right); break;
                case "OR": value = (ushort)(left | right); break;
                case "LSHIFT": value = (ushort)(left << right); break;
                case "RSHIFT": value = (ushort)(left >> right); break;
                default: throw new Exception("Unknown op");
            }
        }

        memo[key] = value;
        return value;
    }

    static void Main()
    {
        foreach (var line in File.ReadLines("input.txt"))
        {
            string[] sides = line.Split(" -> ");
            rules[sides[1]] = sides[0];
        }

        ushort a = Eval("a");
        memo.Clear();
        rules["b"] = a.ToString();
        Console.WriteLine(Eval("a"));
    }
}
