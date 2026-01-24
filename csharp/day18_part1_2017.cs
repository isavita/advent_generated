
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static long GetVal(long[] r, string s) =>
        s[0] >= 'a' && s[0] <= 'z' ? r[s[0] - 'a'] : long.Parse(s);

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var prog = new List<string[]>(lines.Length);
        foreach (var l in lines) prog.Add(l.Split(' ', StringSplitOptions.RemoveEmptyEntries));

        var reg = new long[26];
        long last = 0, recovered = 0;
        int ip = 0;

        while (ip >= 0 && ip < prog.Count)
        {
            var p = prog[ip];
            switch (p[0])
            {
                case "snd": last = GetVal(reg, p[1]); break;
                case "set": reg[p[1][0] - 'a'] = GetVal(reg, p[2]); break;
                case "add": reg[p[1][0] - 'a'] += GetVal(reg, p[2]); break;
                case "mul": reg[p[1][0] - 'a'] *= GetVal(reg, p[2]); break;
                case "mod": reg[p[1][0] - 'a'] %= GetVal(reg, p[2]); break;
                case "rcv":
                    if (GetVal(reg, p[1]) != 0) { recovered = last; Console.WriteLine(recovered); return; }
                    break;
                case "jgz":
                    if (GetVal(reg, p[1]) > 0) ip += (int)GetVal(reg, p[2]) - 1;
                    break;
            }
            ip++;
        }
    }
}
