
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Day23
{
    public static void Main(string[] args)
    {
        var input = File.ReadAllText("input.txt");
        var program = input.Split(',').Select(long.Parse).ToArray();

        Console.WriteLine("Part 1: " + Part1(program));
        Console.WriteLine("Part 2: " + Part2(program));
    }

    static long Part1(long[] program)
    {
        var computers = new IntcodeComputer[50];
        var queues = new Queue<long>[50];
        for (int i = 0; i < 50; i++)
        {
            computers[i] = new IntcodeComputer(program);
            computers[i].AddInput(i);
            queues[i] = new Queue<long>();
        }

        while (true)
        {
            for (int i = 0; i < 50; i++)
            {
                if (queues[i].Count == 0)
                {
                    computers[i].AddInput(-1);
                }
                else
                {
                    computers[i].AddInput(queues[i].Dequeue());
                    computers[i].AddInput(queues[i].Dequeue());
                }

                var output = computers[i].Run();
                for (int j = 0; j < output.Count; j += 3)
                {
                    var address = output[j];
                    var x = output[j + 1];
                    var y = output[j + 2];
                    if (address == 255)
                    {
                        return y;
                    }
                    queues[address].Enqueue(x);
                    queues[address].Enqueue(y);
                }
            }
        }
    }

    static long Part2(long[] program)
    {
        var computers = new IntcodeComputer[50];
        var queues = new Queue<long>[50];
        for (int i = 0; i < 50; i++)
        {
            computers[i] = new IntcodeComputer(program);
            computers[i].AddInput(i);
            queues[i] = new Queue<long>();
        }

        long natX = 0, natY = 0;
        long lastDeliveredY = -1;
        int idleCount = 0;

        while (true)
        {
            bool idle = true;
            for (int i = 0; i < 50; i++)
            {
                if (queues[i].Count == 0)
                {
                    computers[i].AddInput(-1);
                }
                else
                {
                    idle = false;
                    computers[i].AddInput(queues[i].Dequeue());
                    computers[i].AddInput(queues[i].Dequeue());
                }

                var output = computers[i].Run();
                for (int j = 0; j < output.Count; j += 3)
                {
                    idle = false;
                    var address = output[j];
                    var x = output[j + 1];
                    var y = output[j + 2];
                    if (address == 255)
                    {
                        natX = x;
                        natY = y;
                    }
                    else
                    {
                        queues[address].Enqueue(x);
                        queues[address].Enqueue(y);
                    }
                }
            }

            if (idle)
            {
                idleCount++;
                if (idleCount > 100)
                {
                    if (natY == lastDeliveredY)
                    {
                        return natY;
                    }
                    queues[0].Enqueue(natX);
                    queues[0].Enqueue(natY);
                    lastDeliveredY = natY;
                    idleCount = 0;
                }
            }
            else
            {
                idleCount = 0;
            }
        }
    }
}

public class IntcodeComputer
{
    private long[] memory;
    private int pointer = 0;
    private int relativeBase = 0;
    private Queue<long> inputQueue = new Queue<long>();
    private List<long> output = new List<long>();

    public IntcodeComputer(long[] program)
    {
        memory = new long[10000];
        Array.Copy(program, memory, program.Length);
    }

    public void AddInput(long input)
    {
        inputQueue.Enqueue(input);
    }

    public List<long> Run()
    {
        output.Clear();
        while (true)
        {
            var instruction = memory[pointer];
            var opcode = (int)(instruction % 100);
            var mode1 = (int)((instruction / 100) % 10);
            var mode2 = (int)((instruction / 1000) % 10);
            var mode3 = (int)((instruction / 10000) % 10);

            if (opcode == 99)
            {
                break;
            }

            switch (opcode)
            {
                case 1:
                case 2:
                case 7:
                case 8:
                    var param1 = GetParam(mode1, pointer + 1);
                    var param2 = GetParam(mode2, pointer + 2);
                    var writeAddress = GetWriteAddress(mode3, pointer + 3);
                    if (opcode == 1) memory[writeAddress] = param1 + param2;
                    else if (opcode == 2) memory[writeAddress] = param1 * param2;
                    else if (opcode == 7) memory[writeAddress] = param1 < param2 ? 1 : 0;
                    else memory[writeAddress] = param1 == param2 ? 1 : 0;
                    pointer += 4;
                    break;
                case 3:
                    if (inputQueue.Count == 0)
                    {
                        return output;
                    }
                    memory[GetWriteAddress(mode1, pointer + 1)] = inputQueue.Dequeue();
                    pointer += 2;
                    break;
                case 4:
                    output.Add(GetParam(mode1, pointer + 1));
                    pointer += 2;
                    break;
                case 5:
                    param1 = GetParam(mode1, pointer + 1);
                    param2 = GetParam(mode2, pointer + 2);
                    if (param1 != 0) pointer = (int)param2;
                    else pointer += 3;
                    break;
                case 6:
                    param1 = GetParam(mode1, pointer + 1);
                    param2 = GetParam(mode2, pointer + 2);
                    if (param1 == 0) pointer = (int)param2;
                    else pointer += 3;
                    break;
                case 9:
                    relativeBase += (int)GetParam(mode1, pointer + 1);
                    pointer += 2;
                    break;
                default:
                    throw new Exception("Invalid opcode: " + opcode);
            }
        }
        return output;
    }

    private long GetParam(int mode, int address)
    {
        switch (mode)
        {
            case 0:
                return memory[memory[address]];
            case 1:
                return memory[address];
            case 2:
                return memory[relativeBase + memory[address]];
            default:
                throw new Exception("Invalid parameter mode: " + mode);
        }
    }

    private int GetWriteAddress(int mode, int address)
    {
        return mode == 2 ? relativeBase + (int)memory[address] : (int)memory[address];
    }
}
