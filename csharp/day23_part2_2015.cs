using System;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var instr = new string[lines.Length];
        int n = 0;
        foreach (var l in lines)
        {
            var s = l.Trim();
            if (s.Length > 0) instr[n++] = s;
        }

        ulong a = 1, b = 0;
        int ip = 0;
        while (ip >= 0 && ip < n)
        {
            var line = instr[ip];
            var op = line.Substring(0, 3);
            switch (op)
            {
                case "hlf":
                    if (line[4] == 'a') a >>= 1; else b >>= 1;
                    ip++;
                    break;
                case "tpl":
                    if (line[4] == 'a') a *= 3; else b *= 3;
                    ip++;
                    break;
                case "inc":
                    if (line[4] == 'a') a++; else b++;
                    ip++;
                    break;
                case "jmp":
                    ip += int.Parse(line.Substring(4));
                    break;
                case "jie":
                    {
                        char r = line[4];
                        int off = int.Parse(line.Substring(7));
                        ulong val = r == 'a' ? a : b;
                        ip = (val & 1) == 0 ? ip + off : ip + 1;
                    }
                    break;
                case "jio":
                    {
                        char r = line[4];
                        int off = int.Parse(line.Substring(7));
                        ulong val = r == 'a' ? a : b;
                        ip = val == 1 ? ip + off : ip + 1;
                    }
                    break;
                default:
                    return;
            }
        }
        Console.WriteLine(b);
    }
}