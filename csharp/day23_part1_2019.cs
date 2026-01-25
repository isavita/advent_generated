
using System;
using System.IO;
using System.Linq;

public class Program
{
    private const int MemSize = 8192;
    private const int NumComputers = 50;
    private const int QueueCapacity = 256;

    private struct Packet
    {
        public long X { get; set; }
        public long Y { get; set; }
    }

    private class Queue<T>
    {
        private T[] items;
        private int head;
        private int tail;
        private int count;

        public Queue(int capacity)
        {
            items = new T[capacity];
            head = 0;
            tail = 0;
            count = 0;
        }

        public bool IsEmpty => count == 0;

        public void Enqueue(T value)
        {
            if (count < items.Length)
            {
                items[tail] = value;
                tail = (tail + 1) % items.Length;
                count++;
            }
        }

        public T Dequeue()
        {
            if (count > 0)
            {
                T value = items[head];
                head = (head + 1) % items.Length;
                count--;
                return value;
            }
            throw new InvalidOperationException("Queue is empty");
        }
    }

    private class Computer
    {
        private long[] memory;
        public long Ip { get; set; }
        public long RelativeBase { get; set; }
        public Queue<long> InputQueue { get; set; }
        public long[] OutputBuffer { get; set; }
        public int OutputCount { get; set; }
        public bool Halted { get; set; }
        public bool NeedsInput { get; set; }

        public Computer(long[] initialProgram)
        {
            memory = new long[MemSize];
            Array.Copy(initialProgram, memory, initialProgram.Length);
            Ip = 0;
            RelativeBase = 0;
            InputQueue = new Queue<long>(QueueCapacity);
            OutputBuffer = new long[3];
            OutputCount = 0;
            Halted = false;
            NeedsInput = false;
        }

        private long GetMem(long addr)
        {
            if (addr < 0 || addr >= MemSize)
            {
                throw new IndexOutOfRangeException($"Memory access out of bounds: {addr}");
            }
            return memory[addr];
        }

        private void SetMem(long addr, long value)
        {
            if (addr < 0 || addr >= MemSize)
            {
                throw new IndexOutOfRangeException($"Memory access out of bounds: {addr}");
            }
            memory[addr] = value;
        }

        private long GetParam(int mode, int offset)
        {
            long val = GetMem(Ip + offset);
            switch (mode)
            {
                case 0: // Position mode
                    return GetMem(val);
                case 1: // Immediate mode
                    return val;
                case 2: // Relative mode
                    return GetMem(RelativeBase + val);
                default:
                    throw new InvalidOperationException($"Unknown parameter mode: {mode}");
            }
        }

        private void SetParam(int mode, int offset, long value)
        {
            long addr = GetMem(Ip + offset);
            switch (mode)
            {
                case 0: // Position mode
                    SetMem(addr, value);
                    break;
                case 2: // Relative mode
                    SetMem(RelativeBase + addr, value);
                    break;
                default:
                    throw new InvalidOperationException($"Unknown parameter mode for set: {mode}");
            }
        }

        public void Run()
        {
            NeedsInput = false;
            while (!Halted)
            {
                long instruction = GetMem(Ip);
                int opcode = (int)(instruction % 100);
                int[] modes = new int[]
                {
                    (int)((instruction / 100) % 10),
                    (int)((instruction / 1000) % 10),
                    (int)((instruction / 10000) % 10)
                };

                long p1, p2;

                switch (opcode)
                {
                    case 1: // add
                    case 2: // multiply
                    case 7: // less than
                    case 8: // equals
                        p1 = GetParam(modes[0], 1);
                        p2 = GetParam(modes[1], 2);
                        long result;
                        if (opcode == 1) result = p1 + p2;
                        else if (opcode == 2) result = p1 * p2;
                        else if (opcode == 7) result = (p1 < p2) ? 1 : 0;
                        else result = (p1 == p2) ? 1 : 0;
                        SetParam(modes[2], 3, result);
                        Ip += 4;
                        break;
                    case 3: // input
                        if (InputQueue.IsEmpty)
                        {
                            NeedsInput = true;
                            return; // Wait for input
                        }
                        long inputVal = InputQueue.Dequeue();
                        SetParam(modes[0], 1, inputVal);
                        Ip += 2;
                        break;
                    case 4: // output
                        p1 = GetParam(modes[0], 1);
                        OutputBuffer[OutputCount++] = p1;
                        Ip += 2;
                        if (OutputCount == 3)
                        {
                            return; // Output ready
                        }
                        break;
                    case 5: // jump-if-true
                    case 6: // jump-if-false
                        p1 = GetParam(modes[0], 1);
                        p2 = GetParam(modes[1], 2);
                        if ((opcode == 5 && p1 != 0) || (opcode == 6 && p1 == 0))
                        {
                            Ip = p2;
                        }
                        else
                        {
                            Ip += 3;
                        }
                        break;
                    case 9: // adjust relative base
                        p1 = GetParam(modes[0], 1);
                        RelativeBase += p1;
                        Ip += 2;
                        break;
                    case 99: // halt
                        Halted = true;
                        return;
                    default:
                        throw new InvalidOperationException($"Unknown opcode {opcode} at ip {Ip}");
                }
            }
        }
    }

    public static void Main()
    {
        string[] input = File.ReadAllText("input.txt").Split(',');
        long[] initialProgram = input.Select(long.Parse).ToArray();

        Computer[] computers = new Computer[NumComputers];
        Queue<Packet>[] packetQueues = new Queue<Packet>[NumComputers];

        for (int i = 0; i < NumComputers; ++i)
        {
            computers[i] = new Computer(initialProgram);
            computers[i].InputQueue.Enqueue(i); // Initial address input
            packetQueues[i] = new Queue<Packet>(QueueCapacity);
        }

        while (true)
        {
            for (int i = 0; i < NumComputers; ++i)
            {
                if (computers[i].NeedsInput && computers[i].InputQueue.IsEmpty)
                {
                    if (packetQueues[i].IsEmpty)
                    {
                        computers[i].InputQueue.Enqueue(-1);
                    }
                    else
                    {
                        Packet p = packetQueues[i].Dequeue();
                        computers[i].InputQueue.Enqueue(p.X);
                        computers[i].InputQueue.Enqueue(p.Y);
                    }
                }

                computers[i].Run();

                if (computers[i].OutputCount == 3)
                {
                    long dest = computers[i].OutputBuffer[0];
                    long xVal = computers[i].OutputBuffer[1];
                    long yVal = computers[i].OutputBuffer[2];
                    computers[i].OutputCount = 0; // Reset buffer

                    if (dest == 255)
                    {
                        Console.WriteLine(yVal);
                        return;
                    }
                    else if (dest >= 0 && dest < NumComputers)
                    {
                        Packet p = new Packet { X = xVal, Y = yVal };
                        packetQueues[dest].Enqueue(p);
                    }
                }
            }
        }
    }
}
