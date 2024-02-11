
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Solution
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        List<OP> opcodes = new List<OP>
        {
            new OP { Name = "addr", Action = '+', A = 'r', B = 'r' },
            new OP { Name = "addi", Action = '+', A = 'r', B = 'v' },
            new OP { Name = "mulr", Action = '*', A = 'r', B = 'r' },
            new OP { Name = "muli", Action = '*', A = 'r', B = 'v' },
            new OP { Name = "banr", Action = '&', A = 'r', B = 'r' },
            new OP { Name = "bani", Action = '&', A = 'r', B = 'v' },
            new OP { Name = "borr", Action = '|', A = 'r', B = 'r' },
            new OP { Name = "bori", Action = '|', A = 'r', B = 'v' },
            new OP { Name = "setr", Action = 'a', A = 'r', B = 'r' },
            new OP { Name = "seti", Action = 'a', A = 'v', B = 'r' },
            new OP { Name = "gtir", Action = '>', A = 'v', B = 'r' },
            new OP { Name = "gtri", Action = '>', A = 'r', B = 'v' },
            new OP { Name = "gtrr", Action = '>', A = 'r', B = 'r' },
            new OP { Name = "eqir", Action = '=', A = 'v', B = 'r' },
            new OP { Name = "eqri", Action = '=', A = 'r', B = 'v' },
            new OP { Name = "eqir", Action = '=', A = 'r', B = 'r' }
        };

        int sum = 0;
        int lineCount = 0;

        while (lineCount < lines.Length)
        {
            if (lines[lineCount].Length > 0 && lines[lineCount][0] == 'B')
            {
                string[] split = Regex.Split(lines[lineCount], "[^0-9]+");
                int[] registers = { int.Parse(split[1]), int.Parse(split[2]), int.Parse(split[3]), int.Parse(split[4]) };

                split = Regex.Split(lines[lineCount + 1], "[^0-9]+");
                byte[] instruction = { byte.Parse(split[0]), byte.Parse(split[1]), byte.Parse(split[2]), byte.Parse(split[3]) };

                split = Regex.Split(lines[lineCount + 2], "[^0-9]+");
                int[] result = { int.Parse(split[1]), int.Parse(split[2]), int.Parse(split[3]), int.Parse(split[4]) };

                int tempSum = TestCode(registers, result, instruction, opcodes);

                if (tempSum >= 3)
                {
                    sum++;
                }

                lineCount += 4;
            }
            else
            {
                break;
            }
        }

        Dictionary<byte, OP> orderedOpCodes = new Dictionary<byte, OP>();

        while (orderedOpCodes.Count < 16)
        {
            foreach (OP op in opcodes)
            {
                if (op.MatchCount.Count == 1)
                {
                    byte c = op.MatchCount[0];
                    orderedOpCodes[c] = op;
                    foreach (OP op2 in opcodes)
                    {
                        Remove(op2, c);
                    }
                }
            }
        }

        lineCount += 2;
        int[] r = new int[4];

        for (; lineCount < lines.Length; lineCount++)
        {
            string[] split = Regex.Split(lines[lineCount], "[^0-9]+");
            byte[] instruction = { byte.Parse(split[0]), byte.Parse(split[1]), byte.Parse(split[2]), byte.Parse(split[3]) };

            r = RunOp(orderedOpCodes[instruction[0]], r, instruction);
        }

        Console.WriteLine(r[0]);
    }

    static void Remove(OP op, byte c)
    {
        int i = -1;
        for (int j = 0; j < op.MatchCount.Count; j++)
        {
            if (c == op.MatchCount[j])
            {
                i = j;
            }
        }
        if (i != -1)
        {
            op.MatchCount.RemoveAt(i);
        }
    }

    static void Add(OP op, byte c)
    {
        if (!op.MatchCount.Contains(c))
        {
            op.MatchCount.Add(c);
        }
    }

    static int TestCode(int[] registers, int[] result, byte[] instruction, List<OP> opcodes)
    {
        int sum = 0;
        foreach (OP op in opcodes)
        {
            if (Match(result, RunOp(op, registers, instruction)))
            {
                Add(op, instruction[0]);
                sum++;
            }
        }
        return sum;
    }

    static bool Match(int[] r, int[] c)
    {
        if (r.Length != c.Length)
        {
            return false;
        }
        for (int i = 0; i < r.Length; i++)
        {
            if (r[i] != c[i])
            {
                return false;
            }
        }
        return true;
    }

    static int[] RunOp(OP op, int[] registers, byte[] instruction)
    {
        int[] registerCP = new int[4];
        Array.Copy(registers, registerCP, 4);
        int A, B;
        if (op.A == 'r')
        {
            A = registerCP[instruction[1]];
        }
        else
        {
            A = instruction[1];
        }
        if (op.B == 'r')
        {
            B = registerCP[instruction[2]];
        }
        else
        {
            B = instruction[2];
        }
        switch (op.Action)
        {
            case '+':
                registerCP[instruction[3]] = A + B;
                break;
            case '*':
                registerCP[instruction[3]] = A * B;
                break;
            case '&':
                registerCP[instruction[3]] = A & B;
                break;
            case '|':
                registerCP[instruction[3]] = A | B;
                break;
            case 'a':
                registerCP[instruction[3]] = A;
                break;
            case '>':
                registerCP[instruction[3]] = (A > B) ? 1 : 0;
                break;
            case '=':
                registerCP[instruction[3]] = (A == B) ? 1 : 0;
                break;
            default:
                Console.WriteLine("not valid instruction");
                break;
        }
        return registerCP;
    }
}

class OP
{
    public char A { get; set; }
    public char B { get; set; }
    public char Action { get; set; }
    public string Name { get; set; }
    public List<byte> MatchCount { get; set; }

    public OP()
    {
        MatchCount = new List<byte>();
    }
}
