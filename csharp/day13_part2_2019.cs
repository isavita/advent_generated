using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class IntcodeComputer
{
    private Dictionary<long, long> memory = new Dictionary<long, long>();
    private long ip = 0;
    private long relativeBase = 0;
    private bool awaitingInput = false;

    public IntcodeComputer(long[] program)
    {
        for (long i = 0; i < program.Length; i++) memory[i] = program[i];
    }

    private long GetMemory(long address) => memory.TryGetValue(address, out var v) ? v : 0L;
    private void SetMemoryAt(long address, long value) => memory[address] = value;

    public void SetMemory(long address, long value) => SetMemoryAt(address, value);

    private long GetParameter(int offset, int mode)
    {
        long val = GetMemory(ip + offset);
        switch (mode)
        {
            case 0: return GetMemory(val);
            case 1: return val;
            case 2: return GetMemory(relativeBase + val);
            default: throw new InvalidOperationException("Unknown parameter mode");
        }
    }

    private long GetWriteAddress(int offset, int mode)
    {
        long val = GetMemory(ip + offset);
        switch (mode)
        {
            case 0: return val;
            case 2: return relativeBase + val;
            default: throw new InvalidOperationException("Invalid write address mode");
        }
    }

    public IEnumerable<long> Execute(Func<long> inputProvider)
    {
        while (true)
        {
            long instruction = GetMemory(ip);
            int opcode = (int)(instruction % 100);
            int[] modes = new int[3]
            {
                (int)((instruction / 100) % 10),
                (int)((instruction / 1000) % 10),
                (int)((instruction / 10000) % 10)
            };

            switch (opcode)
            {
                case 99:
                    yield break;

                case 3:
                    if (!awaitingInput)
                    {
                        long inputValue = inputProvider();
                        SetMemory(GetWriteAddress(1, modes[0]), inputValue);
                        awaitingInput = true;
                    }
                    ip += 2;
                    break;

                case 4:
                    long outputValue = GetParameter(1, modes[0]);
                    ip += 2;
                    awaitingInput = false;
                    yield return outputValue;
                    break;

                case 1:
                {
                    long p1 = GetParameter(1, modes[0]);
                    long p2 = GetParameter(2, modes[1]);
                    SetMemory(GetWriteAddress(3, modes[2]), p1 + p2);
                    ip += 4;
                    break;
                }
                case 2:
                {
                    long p1 = GetParameter(1, modes[0]);
                    long p2 = GetParameter(2, modes[1]);
                    SetMemory(GetWriteAddress(3, modes[2]), p1 * p2);
                    ip += 4;
                    break;
                }
                case 5:
                {
                    long p1 = GetParameter(1, modes[0]);
                    long p2 = GetParameter(2, modes[1]);
                    if (p1 != 0) ip = p2; else ip += 3;
                    break;
                }
                case 6:
                {
                    long p1 = GetParameter(1, modes[0]);
                    long p2 = GetParameter(2, modes[1]);
                    if (p1 == 0) ip = p2; else ip += 3;
                    break;
                }
                case 7:
                {
                    long p1 = GetParameter(1, modes[0]);
                    long p2 = GetParameter(2, modes[1]);
                    SetMemory(GetWriteAddress(3, modes[2]), p1 < p2 ? 1 : 0);
                    ip += 4;
                    break;
                }
                case 8:
                {
                    long p1 = GetParameter(1, modes[0]);
                    long p2 = GetParameter(2, modes[1]);
                    SetMemory(GetWriteAddress(3, modes[2]), p1 == p2 ? 1 : 0);
                    ip += 4;
                    break;
                }
                case 9:
                {
                    long p1 = GetParameter(1, modes[0]);
                    relativeBase += p1;
                    ip += 2;
                    break;
                }
                default:
                    throw new InvalidOperationException("Unknown opcode " + opcode);
            }
        }
    }
}

class Program
{
    static void Main(string[] args)
    {
        var program = ParseInput("input.txt");
        long score = PlayGame(program);
        Console.WriteLine("Final Score: " + score);
    }

    static long[] ParseInput(string path)
    {
        var text = File.ReadAllText(path).Trim();
        return text.Split(',', StringSplitOptions.RemoveEmptyEntries).Select(s => long.Parse(s)).ToArray();
    }

    static long PlayGame(long[] program)
    {
        var computer = new IntcodeComputer(program);
        computer.SetMemory(0, 2); // free play mode

        long score = 0;
        long ballX = 0;
        long paddleX = 0;

        Func<long> inputProvider = () =>
        {
            long diff = ballX - paddleX;
            if (diff > 0) return 1;
            if (diff < 0) return -1;
            return 0;
        };

        var en = computer.Execute(inputProvider).GetEnumerator();
        while (en.MoveNext())
        {
            long x = en.Current;
            en.MoveNext();
            long y = en.Current;
            en.MoveNext();
            long tile = en.Current;

            if (x == -1 && y == 0)
            {
                score = tile;
            }
            else
            {
                if (tile == 3) paddleX = x;
                else if (tile == 4) ballX = x;
            }
        }

        return score;
    }
}