using System;
using System.IO;

class Program
{
    struct Instruction
    {
        public string Op;
        public int Arg1Type; //0 reg,1 imm
        public int Arg1Val;
        public int Arg2Type;
        public int Arg2Val;
    }

    static bool IsReg(string s) => s.Length == 1 && s[0] >= 'a' && s[0] <= 'd';
    static int RegIdx(char c) => c - 'a';

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var instr = new Instruction[lines.Length];
        int n = 0;
        foreach (var raw in lines)
        {
            var parts = raw.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            var i = new Instruction { Op = parts[0] };
            if (parts.Length > 1)
            {
                if (IsReg(parts[1]))
                {
                    i.Arg1Type = 0;
                    i.Arg1Val = RegIdx(parts[1][0]);
                }
                else
                {
                    i.Arg1Type = 1;
                    i.Arg1Val = int.Parse(parts[1]);
                }
                if (parts.Length > 2)
                {
                    if (IsReg(parts[2]))
                    {
                        i.Arg2Type = 0;
                        i.Arg2Val = RegIdx(parts[2][0]);
                    }
                    else
                    {
                        i.Arg2Type = 1;
                        i.Arg2Val = int.Parse(parts[2]);
                    }
                }
            }
            instr[n++] = i;
        }

        var reg = new int[4] { 12, 0, 0, 0 };
        int pc = 0;
        while (pc < n)
        {
            var cur = instr[pc];
            int val1 = cur.Arg1Type == 0 ? reg[cur.Arg1Val] : cur.Arg1Val;
            if (cur.Op == "cpy")
            {
                reg[cur.Arg2Val] = val1;
            }
            else if (cur.Op == "inc")
            {
                reg[cur.Arg1Val]++;
            }
            else if (cur.Op == "dec")
            {
                reg[cur.Arg1Val]--;
            }
            else if (cur.Op == "jnz")
            {
                if (val1 != 0)
                {
                    int offset = cur.Arg2Type == 0 ? reg[cur.Arg2Val] : cur.Arg2Val;
                    pc += offset;
                    continue;
                }
            }
            else if (cur.Op == "tgl")
            {
                int target = pc + val1;
                if (target >= 0 && target < n)
                {
                    ref var tgt = ref instr[target];
                    if (tgt.Op == "inc") tgt.Op = "dec";
                    else if (tgt.Op == "dec" || tgt.Op == "tgl") tgt.Op = "inc";
                    else if (tgt.Op == "jnz") tgt.Op = "cpy";
                    else if (tgt.Op == "cpy") tgt.Op = "jnz";
                }
            }
            pc++;
        }

        Console.WriteLine(reg[0]);
    }
}
