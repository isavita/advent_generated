
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Prog
{
    public long A, B, C;
    public int[] Program;
    public int ProgramLen;
}

class Solver
{
    static long ComputeOperand(int val, long a, long b, long c)
    {
        switch (val)
        {
            case 0:
            case 1:
            case 2:
            case 3:
                return val;
            case 4:
                return a;
            case 5:
                return b;
            case 6:
                return c;
            default:
                throw new InvalidOperationException("Invalid combo operand: " + val);
        }
    }

    static int[] SimulateComputer(Prog prog, out int outLen)
    {
        long a = prog.A;
        long b = prog.B;
        long c = prog.C;
        int[] input = prog.Program;
        int inputLen = prog.ProgramLen;

        List<int> outs = new List<int>();

        for (int i = 0; i < inputLen; i += 2)
        {
            int cmd = input[i];
            switch (cmd)
            {
                case 0:
                    a >>= (int)ComputeOperand(input[i + 1], a, b, c);
                    break;
                case 1:
                    b ^= input[i + 1];
                    break;
                case 2:
                    b = ComputeOperand(input[i + 1], a, b, c) % 8;
                    break;
                case 3:
                    if (a != 0)
                        i = input[i + 1] - 2;
                    break;
                case 4:
                    b ^= c;
                    break;
                case 5:
                    int val = (int)(ComputeOperand(input[i + 1], a, b, c) % 8);
                    outs.Add(val);
                    break;
                case 6:
                    b = a >> (int)ComputeOperand(input[i + 1], a, b, c);
                    break;
                case 7:
                    c = a >> (int)ComputeOperand(input[i + 1], a, b, c);
                    break;
                default:
                    throw new InvalidOperationException("Invalid opcode: " + cmd);
            }
        }

        outLen = outs.Count;
        return outs.ToArray();
    }

    static long[] Check(Prog p, out int validLen)
    {
        List<long> valids = new List<long>();
        validLen = 0;

        var stack = new Stack<(int Depth, long Score)>();
        var seen = new HashSet<(int, long)>();

        stack.Push((0, 0));

        while (stack.Count > 0)
        {
            var state = stack.Pop();

            if (seen.Contains((state.Depth, state.Score)))
                continue;
            seen.Add((state.Depth, state.Score));

            int depth = state.Depth;
            long score = state.Score;

            if (depth == p.ProgramLen)
            {
                valids.Add(score);
            }
            else
            {
                for (long i = 0; i < 8; i++)
                {
                    long newScore = i + 8 * score;
                    Prog testProg = new Prog { A = newScore, B = p.B, C = p.C, Program = p.Program, ProgramLen = p.ProgramLen };
                    int outLen;
                    int[] result = SimulateComputer(testProg, out outLen);
                    if (outLen > 0 && result[0] == p.Program[p.ProgramLen - 1 - depth])
                    {
                        stack.Push((depth + 1, newScore));
                    }
                }
            }
        }

        validLen = valids.Count;
        return valids.ToArray();
    }

    static void Main()
    {
        string path = "input.txt";
        if (!File.Exists(path))
        {
            Console.Error.WriteLine("Input file not found");
            return;
        }

        long a = 0, b = 0, c = 0;
        int[] program = null;
        int programLen = 0;

        foreach (var line in File.ReadLines(path))
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            var parts = line.Split(new[] { ':' }, 2);
            if (parts.Length < 2) continue;

            string prefix = parts[0].Trim();
            string value = parts[1].Trim();

            if (prefix == "Register A")
                a = long.Parse(value);
            else if (prefix == "Register B")
                b = long.Parse(value);
            else if (prefix == "Register C")
                c = long.Parse(value);
            else if (prefix == "Program")
            {
                var nums = value.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
                program = nums.Select(s => int.Parse(s.Trim())).ToArray();
                programLen = program.Length;
            }
        }

        Prog p = new Prog { A = a, B = b, C = c, Program = program, ProgramLen = programLen };

        int validLen;
        long[] validValues = Check(p, out validLen);

        if (validLen == 0)
        {
            Console.WriteLine("No valid values found");
            return;
        }

        long minVal = validValues[0];
        for (int i = 1; i < validLen; i++)
        {
            if (validValues[i] < minVal)
                minVal = validValues[i];
        }

        Console.WriteLine(minVal);
    }
}
