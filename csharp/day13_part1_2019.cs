
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Machine
{
    private Dictionary<long, long> data;
    private long ip;
    private long relbase;
    private Queue<long> input;
    private Queue<long> output;

    public Machine(long[] program, Queue<long> input, Queue<long> output)
    {
        data = new Dictionary<long, long>();
        for (long i = 0; i < program.Length; i++)
        {
            data[i] = program[i];
        }
        this.input = input;
        this.output = output;
    }

    private long Get(long i, int mode)
    {
        switch (mode)
        {
            case 0:
                return data.ContainsKey(data[i]) ? data[data[i]] : 0;
            case 1:
                return data.ContainsKey(i) ? data[i] : 0;
            case 2:
                return data.ContainsKey(relbase + data[i]) ? data[relbase + data[i]] : 0;
            default:
                throw new Exception("Unknown mode: " + mode);
        }
    }

    private void Set(long i, int mode, long val)
    {
        switch (mode)
        {
            case 0:
                data[data[i]] = val;
                break;
            case 2:
                data[relbase + data[i]] = val;
                break;
            default:
                throw new Exception("Unknown mode: " + mode);
        }
    }

    public void Run()
    {
        while (Step()) { }
    }

    private bool Step()
    {
        long op = data[ip];
        int[] modes = new int[3];
        for (int i = 0; i < 3; i++)
        {
            modes[i] = (int)((op / (long)Math.Pow(10, i + 2)) % 10);
        }
        op %= 100;

        switch (op)
        {
            case 1:
                long val = Get(ip + 1, modes[0]) + Get(ip + 2, modes[1]);
                Set(ip + 3, modes[2], val);
                ip += 4;
                break;
            case 2:
                val = Get(ip + 1, modes[0]) * Get(ip + 2, modes[1]);
                Set(ip + 3, modes[2], val);
                ip += 4;
                break;
            case 3:
                if (!input.TryDequeue(out long inputVal))
                {
                    throw new Exception("Input queue is empty");
                }
                Set(ip + 1, modes[0], inputVal);
                ip += 2;
                break;
            case 4:
                output.Enqueue(Get(ip + 1, modes[0]));
                ip += 2;
                break;
            case 5:
                if (Get(ip + 1, modes[0]) != 0)
                {
                    ip = Get(ip + 2, modes[1]);
                }
                else
                {
                    ip += 3;
                }
                break;
            case 6:
                if (Get(ip + 1, modes[0]) == 0)
                {
                    ip = Get(ip + 2, modes[1]);
                }
                else
                {
                    ip += 3;
                }
                break;
            case 7:
                if (Get(ip + 1, modes[0]) < Get(ip + 2, modes[1]))
                {
                    Set(ip + 3, modes[2], 1);
                }
                else
                {
                    Set(ip + 3, modes[2], 0);
                }
                ip += 4;
                break;
            case 8:
                if (Get(ip + 1, modes[0]) == Get(ip + 2, modes[1]))
                {
                    Set(ip + 3, modes[2], 1);
                }
                else
                {
                    Set(ip + 3, modes[2], 0);
                }
                ip += 4;
                break;
            case 9:
                relbase += Get(ip + 1, modes[0]);
                ip += 2;
                break;
            case 99:
                return false;
            default:
                throw new Exception("Unknown opcode: " + op);
        }
        return true;
    }
}

public class Program
{
    public static int CountBlocks(long[] program)
    {
        Dictionary<string, long> grid = new Dictionary<string, long>();
        Queue<long> input = new Queue<long>();
        Queue<long> output = new Queue<long>();
        Machine machine = new Machine(program, input, output);
        machine.Run();
        while (output.Count > 0)
        {
            long x = output.Dequeue();
            long y = output.Dequeue();
            grid[$"{x},{y}"] = output.Dequeue();
        }
        return grid.Values.Count(val => val == 2);
    }

    public static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        long[] program = input.Split(',').Select(long.Parse).ToArray();
        Console.WriteLine(CountBlocks(program));
    }
}
