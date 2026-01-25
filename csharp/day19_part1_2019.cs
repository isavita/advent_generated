
using System;
using System.IO;
using System.Linq;

public class IntcodeVM
{
    private long[] code;
    private int ip;
    private long[] input;
    private int inputIndex;
    private long output;
    private long relativeBase;

    public IntcodeVM(long[] code, long[] input)
    {
        this.code = new long[10000];
        Array.Copy(code, this.code, code.Length);
        this.input = input;
        ip = 0;
        relativeBase = 0;
    }

    private long GetParamAddress(int pos, int mode)
    {
        switch (mode)
        {
            case 0: return code[pos];
            case 1: return pos;
            case 2: return relativeBase + code[pos];
            default: throw new Exception("Invalid mode");
        }
    }

    public long Run()
    {
        while (true)
        {
            int cmd = (int)code[ip];
            int opCode = cmd % 100;
            int[] modes = new int[]
            {
                (cmd / 100) % 10,
                (cmd / 1000) % 10,
                (cmd / 10000) % 10
            };

            switch (opCode)
            {
                case 1:
                    code[GetParamAddress(ip + 3, modes[2])] = code[GetParamAddress(ip + 1, modes[0])] + code[GetParamAddress(ip + 2, modes[1])];
                    ip += 4;
                    break;
                case 2:
                    code[GetParamAddress(ip + 3, modes[2])] = code[GetParamAddress(ip + 1, modes[0])] * code[GetParamAddress(ip + 2, modes[1])];
                    ip += 4;
                    break;
                case 3:
                    code[GetParamAddress(ip + 1, modes[0])] = input[inputIndex++];
                    ip += 2;
                    break;
                case 4:
                    output = code[GetParamAddress(ip + 1, modes[0])];
                    ip += 2;
                    return output;
                case 5:
                    if (code[GetParamAddress(ip + 1, modes[0])] != 0)
                    {
                        ip = (int)code[GetParamAddress(ip + 2, modes[1])];
                    }
                    else
                    {
                        ip += 3;
                    }
                    break;
                case 6:
                    if (code[GetParamAddress(ip + 1, modes[0])] == 0)
                    {
                        ip = (int)code[GetParamAddress(ip + 2, modes[1])];
                    }
                    else
                    {
                        ip += 3;
                    }
                    break;
                case 7:
                    code[GetParamAddress(ip + 3, modes[2])] = code[GetParamAddress(ip + 1, modes[0])] < code[GetParamAddress(ip + 2, modes[1])] ? 1 : 0;
                    ip += 4;
                    break;
                case 8:
                    code[GetParamAddress(ip + 3, modes[2])] = code[GetParamAddress(ip + 1, modes[0])] == code[GetParamAddress(ip + 2, modes[1])] ? 1 : 0;
                    ip += 4;
                    break;
                case 9:
                    relativeBase += code[GetParamAddress(ip + 1, modes[0])];
                    ip += 2;
                    break;
                case 99:
                    return -1; // Halt
                default:
                    throw new Exception("Invalid opCode");
            }
        }
    }
}

class Program
{
    static void Main()
    {
        string[] input = File.ReadAllText("input.txt").Split(',');
        long[] code = input.Select(long.Parse).ToArray();

        int sum = 0;
        for (int y = 0; y < 50; y++)
        {
            for (int x = 0; x < 50; x++)
            {
                IntcodeVM vm = new IntcodeVM(code, new long[] { x, y });
                if (vm.Run() == 1)
                {
                    sum++;
                }
            }
        }

        Console.WriteLine(sum);
    }
}
