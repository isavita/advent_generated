
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static Dictionary<string, string> rules = new Dictionary<string, string>();
    static Dictionary<string, ushort> memo = new Dictionary<string, ushort>();

    static ushort Eval(string key)
    {
        if (memo.TryGetValue(key, out var cached)) return cached;

        ushort result;
        if (ushort.TryParse(key, out result))
        {
            memo[key] = result;
            return result;
        }

        var rule = rules[key];
        var parts = rule.Split(' ');

        if (parts.Length == 1)
            result = Eval(parts[0]);
        else if (parts[0] == "NOT")
            result = (ushort)~Eval(parts[1]);
        else
        {
            var left = Eval(parts[0]);
            var right = Eval(parts[2]);
            switch (parts[1])
            {
                case "AND": result = (ushort)(left & right); break;
                case "OR": result = (ushort)(left | right); break;
                case "LSHIFT": result = (ushort)(left << right); break;
                case "RSHIFT": result = (ushort)(left >> right); break;
                default: throw new Exception("Invalid op");
            }
        }

        memo[key] = result;
        return result;
    }

    static void Main()
    {
        foreach (var line in File.ReadAllLines("input.txt"))
        {
            var sides = line.Split(" -> ");
            rules[sides[1]] = sides[0];
        }
        Console.WriteLine(Eval("a"));
    }
}
