using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static Dictionary<long, long> mem = new Dictionary<long, long>();
    static long relativeBase = 0;

    static long Read(long addr)
    {
        if (addr < 0) throw new Exception("Negative address");
        return mem.TryGetValue(addr, out long v) ? v : 0;
    }

    static void Write(long addr, long val)
    {
        mem[addr] = val;
    }

    static long GetParam(int mode, long pos)
    {
        long param = Read(pos);
        switch (mode)
        {
            case 0: return Read(param);
            case 1: return param;
            case 2: return Read(param + relativeBase);
            default: throw new Exception("Invalid parameter mode");
        }
    }

    static void SetParam(int mode, long pos, long value)
    {
        long param = Read(pos);
        if (mode == 0) Write(param, value);
        else if (mode == 2) Write(param + relativeBase, value);
        else throw new Exception("Invalid parameter mode for setting");
    }

    static long RunIntcode(long input)
    {
        long ip = 0;
        long output = 0;

        while (true)
        {
            long instr = Read(ip);
            long opcode = instr % 100;
            int mode1 = (int)((instr / 100) % 10);
            int mode2 = (int)((instr / 1000) % 10);
            int mode3 = (int)((instr / 10000) % 10);

            if (opcode == 99) break;

            if (opcode == 1)
            {
                long val = GetParam(mode1, ip + 1) + GetParam(mode2, ip + 2);
                SetParam(mode3, ip + 3, val);
                ip += 4;
            }
            else if (opcode == 2)
            {
                long val = GetParam(mode1, ip + 1) * GetParam(mode2, ip + 2);
                SetParam(mode3, ip + 3, val);
                ip += 4;
            }
            else if (opcode == 3)
            {
                SetParam(mode1, ip + 1, input);
                ip += 2;
            }
            else if (opcode == 4)
            {
                output = GetParam(mode1, ip + 1);
                Console.WriteLine(output);
                ip += 2;
            }
            else if (opcode == 5)
            {
                if (GetParam(mode1, ip + 1) != 0)
                    ip = GetParam(mode2, ip + 2);
                else
                    ip += 3;
            }
            else if (opcode == 6)
            {
                if (GetParam(mode1, ip + 1) == 0)
                    ip = GetParam(mode2, ip + 2);
                else
                    ip += 3;
            }
            else if (opcode == 7)
            {
                long val = (GetParam(mode1, ip + 1) < GetParam(mode2, ip + 2)) ? 1 : 0;
                SetParam(mode3, ip + 3, val);
                ip += 4;
            }
            else if (opcode == 8)
            {
                long val = (GetParam(mode1, ip + 1) == GetParam(mode2, ip + 2)) ? 1 : 0;
                SetParam(mode3, ip + 3, val);
                ip += 4;
            }
            else if (opcode == 9)
            {
                relativeBase += GetParam(mode1, ip + 1);
                ip += 2;
            }
            else
            {
                throw new Exception("Invalid opcode: " + opcode);
            }
        }

        return output;
    }

    static void Main(string[] args)
    {
        string inputPath = "input.txt";
        if (!File.Exists(inputPath))
        {
            Console.Error.WriteLine("Error opening file");
            return;
        }

        string content = File.ReadAllText(inputPath).Trim();
        var tokens = content.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);

        mem.Clear();
        for (long i = 0; i < tokens.Length; i++)
        {
            if (long.TryParse(tokens[i], out long val))
                mem[i] = val;
        }

        relativeBase = 0;
        RunIntcode(1);

        relativeBase = 0;
        RunIntcode(2);
    }
}