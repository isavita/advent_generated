
using System;
using System.IO;

class Program
{
    static int GetValue(string s, int[] regs)
    {
        char c = s[0];
        return char.IsLetter(c) ? regs[c - 'a'] : int.Parse(s);
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int[] regs = new int[26];
        int mul = 0, ip = 0;

        while (ip >= 0 && ip < lines.Length)
        {
            var parts = lines[ip].Split(' ');
            switch (parts[0])
            {
                case "set": regs[parts[1][0] - 'a'] = GetValue(parts[2], regs); break;
                case "sub": regs[parts[1][0] - 'a'] -= GetValue(parts[2], regs); break;
                case "mul": regs[parts[1][0] - 'a'] *= GetValue(parts[2], regs); mul++; break;
                case "jnz":
                    if (GetValue(parts[1], regs) != 0)
                        ip += GetValue(parts[2], regs) - 1;
                    break;
            }
            ip++;
        }
        Console.WriteLine(mul);
    }
}
