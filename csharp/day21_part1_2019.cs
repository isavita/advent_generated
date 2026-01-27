using System;
using System.Collections.Generic;
using System.IO;

class VM
{
    Dictionary<int, long> code;
    int ip;
    int relativeBase;
    Queue<long> input;
    Queue<long> output;

    public VM(string filename)
    {
        code = new Dictionary<int, long>();
        var text = File.ReadAllText(filename).Trim();
        var parts = text.Split(',');
        for (int i = 0; i < parts.Length; i++) code[i] = long.Parse(parts[i]);
        ip = 0;
        relativeBase = 0;
        input = new Queue<long>();
        output = new Queue<long>();
    }

    public void Run()
    {
        while (true)
        {
            long instr = code[ip];
            int opcode = (int)(instr % 100);
            switch (opcode)
            {
                case 1:
                    {
                        var p = GetParamsAddresses(3, instr);
                        code[p[2]] = GetValue(p[0]) + GetValue(p[1]);
                        ip += 4; break;
                    }
                case 2:
                    {
                        var p = GetParamsAddresses(3, instr);
                        code[p[2]] = GetValue(p[0]) * GetValue(p[1]);
                        ip += 4; break;
                    }
                case 3:
                    {
                        var p = GetParamsAddresses(1, instr);
                        code[p[0]] = input.Dequeue();
                        ip += 2; break;
                    }
                case 4:
                    {
                        var p = GetParamsAddresses(1, instr);
                        output.Enqueue(GetValue(p[0]));
                        ip += 2; break;
                    }
                case 5:
                    {
                        var p = GetParamsAddresses(2, instr);
                        if (GetValue(p[0]) != 0) ip = (int)GetValue(p[1]); else ip += 3; break;
                    }
                case 6:
                    {
                        var p = GetParamsAddresses(2, instr);
                        if (GetValue(p[0]) == 0) ip = (int)GetValue(p[1]); else ip += 3; break;
                    }
                case 7:
                    {
                        var p = GetParamsAddresses(3, instr);
                        code[p[2]] = GetValue(p[0]) < GetValue(p[1]) ? 1 : 0;
                        ip += 4; break;
                    }
                case 8:
                    {
                        var p = GetParamsAddresses(3, instr);
                        code[p[2]] = GetValue(p[0]) == GetValue(p[1]) ? 1 : 0;
                        ip += 4; break;
                    }
                case 9:
                    {
                        var p = GetParamsAddresses(1, instr);
                        relativeBase += (int)GetValue(p[0]); ip += 2; break;
                    }
                case 99:
                    return;
                default:
                    throw new InvalidOperationException($"Invalid opcode {opcode}");
            }
        }
    }

    long GetValue(int addr) => code.TryGetValue(addr, out var v) ? v : 0;
    List<int> GetParamsAddresses(int arity, long instr)
    {
        var modes = new int[arity];
        var modeSection = instr / 100;
        for (int i = 0; i < arity; i++)
            modes[i] = (int)((modeSection / (int)Math.Pow(10, i)) % 10);
        var addresses = new List<int>(arity);
        for (int i = 0; i < arity; i++)
        {
            int mode = modes[i];
            int pos = ip + i + 1;
            int addr;
            if (mode == 0) addr = (int)(code.TryGetValue(pos, out var v0) ? v0 : 0);
            else if (mode == 1) addr = pos;
            else addr = relativeBase + (int)(code.TryGetValue(pos, out var v1) ? v1 : 0);
            addresses.Add(addr);
        }
        return addresses;
    }

    public void SendString(string s)
    {
        foreach (char c in s) input.Enqueue((long)c);
        input.Enqueue((long)'\n');
    }

    public long? GetOutput()
    {
        if (output.Count == 0) return null;
        return output.Dequeue();
    }
}

class Program
{
    static void Main()
    {
        var vm = new VM("input.txt");
        string[] instructions = {
            "NOT A J",
            "NOT B T",
            "OR T J",
            "NOT C T",
            "OR T J",
            "AND D J",
            "WALK"
        };
        foreach (var instr in instructions) vm.SendString(instr);
        vm.Run();
        while (true)
        {
            var outv = vm.GetOutput();
            if (!outv.HasValue) break;
            if (outv.Value > 127)
            {
                Console.WriteLine(outv);
                return;
            }
        }
    }
}
