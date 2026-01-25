
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public enum OpCode
{
    SND,
    SET,
    ADD,
    MUL,
    MOD,
    RCV,
    JGZ
}

public struct Argument
{
    public bool IsReg;
    public long Val;
}

public struct Instruction
{
    public OpCode Op;
    public Argument Arg1;
    public Argument Arg2;
}

public class Queue
{
    private long[] data;
    private int head;
    private int tail;
    private int count;

    public Queue(int size)
    {
        data = new long[size];
        head = 0;
        tail = 0;
        count = 0;
    }

    public bool IsEmpty => count == 0;
    public bool IsFull => count == data.Length;

    public void Push(long value)
    {
        if (!IsFull)
        {
            data[tail] = value;
            tail = (tail + 1) % data.Length;
            count++;
        }
        else
        {
            throw new InvalidOperationException("Queue overflow");
        }
    }

    public long Pop()
    {
        if (!IsEmpty)
        {
            long value = data[head];
            head = (head + 1) % data.Length;
            count--;
            return value;
        }
        else
        {
            throw new InvalidOperationException("Queue underflow");
        }
    }
}

class Program
{
    static void Main(string[] args)
    {
        var instructions = File.ReadAllLines("input.txt")
            .Select(ParseInstruction)
            .ToArray();

        var registers = new long[2, 26];
        registers[1, 'p' - 'a'] = 1;

        var queue0 = new Queue(100000);
        var queue1 = new Queue(100000);

        int pc0 = 0, pc1 = 1;
        bool waiting0 = false, waiting1 = false;
        bool terminated0 = false, terminated1 = false;
        long sendCount1 = 0;

        while (!(terminated0 && terminated1) && !(waiting0 && waiting1))
        {
            for (int progId = 0; progId < 2; progId++)
            {
                int pc = progId == 0 ? pc0 : pc1;
                bool waiting = progId == 0 ? waiting0 : waiting1;
                bool terminated = progId == 0 ? terminated0 : terminated1;

                if (terminated || waiting)
                {
                    continue;
                }

                bool executedInstruction = false;
                while (pc >= 0 && pc < instructions.Length)
                {
                    var instr = instructions[pc];
                    int targetReg = instr.Arg1.IsReg ? (int)instr.Arg1.Val : -1;

                    executedInstruction = true;

                    switch (instr.Op)
                    {
                        case OpCode.SND:
                            if (progId == 0)
                            {
                                queue1.Push(GetValue(instr.Arg1, registers, progId));
                            }
                            else
                            {
                                queue0.Push(GetValue(instr.Arg1, registers, progId));
                                sendCount1++;
                            }
                            break;
                        case OpCode.SET:
                            if (targetReg != -1) registers[progId, targetReg] = GetValue(instr.Arg2, registers, progId);
                            break;
                        case OpCode.ADD:
                            if (targetReg != -1) registers[progId, targetReg] += GetValue(instr.Arg2, registers, progId);
                            break;
                        case OpCode.MUL:
                            if (targetReg != -1) registers[progId, targetReg] *= GetValue(instr.Arg2, registers, progId);
                            break;
                        case OpCode.MOD:
                            if (targetReg != -1)
                            {
                                long val2 = GetValue(instr.Arg2, registers, progId);
                                if (val2 != 0)
                                {
                                    registers[progId, targetReg] %= val2;
                                }
                                else
                                {
                                    registers[progId, targetReg] = 0;
                                }
                            }
                            break;
                        case OpCode.RCV:
                            var recvQueue = progId == 0 ? queue0 : queue1;
                            if (recvQueue.IsEmpty)
                            {
                                if (progId == 0) waiting0 = true;
                                else waiting1 = true;
                                executedInstruction = false;
                                goto nextProgram;
                            }
                            else
                            {
                                if (targetReg != -1) registers[progId, targetReg] = recvQueue.Pop();
                                if (progId == 0) waiting0 = false;
                                else waiting1 = false;
                            }
                            break;
                        case OpCode.JGZ:
                            if (GetValue(instr.Arg1, registers, progId) > 0)
                            {
                                pc += (int)GetValue(instr.Arg2, registers, progId);
                                continue;
                            }
                            break;
                    }
                    if (progId == 0) pc0 = pc + 1;
                    else pc1 = pc + 1;
                    pc++;

                    if (instr.Op == OpCode.SND)
                    {
                        int otherProg = 1 - progId;
                        if (otherProg == 0 && waiting0) waiting0 = false;
                        else if (otherProg == 1 && waiting1) waiting1 = false;
                    }
                }

                if (!(pc >= 0 && pc < instructions.Length))
                {
                    if (progId == 0) terminated0 = true;
                    else terminated1 = true;
                }
                if ((progId == 0 && waiting0) || (progId == 1 && waiting1)) executedInstruction = false;

            nextProgram:;
            }
        }

        Console.WriteLine(sendCount1);
    }

    static Instruction ParseInstruction(string line)
    {
        var parts = line.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
        var instr = new Instruction();

        switch (parts[0])
        {
            case "snd": instr.Op = OpCode.SND; break;
            case "set": instr.Op = OpCode.SET; break;
            case "add": instr.Op = OpCode.ADD; break;
            case "mul": instr.Op = OpCode.MUL; break;
            case "mod": instr.Op = OpCode.MOD; break;
            case "rcv": instr.Op = OpCode.RCV; break;
            case "jgz": instr.Op = OpCode.JGZ; break;
        }

        instr.Arg1 = ParseArgument(parts[1]);
        if (parts.Length > 2) instr.Arg2 = ParseArgument(parts[2]);
        else instr.Arg2 = new Argument { IsReg = false, Val = 0 };

        return instr;
    }

    static Argument ParseArgument(string s)
    {
        if (char.IsLetter(s[0]) && s.Length == 1)
        {
            return new Argument { IsReg = true, Val = s[0] - 'a' };
        }
        else
        {
            return new Argument { IsReg = false, Val = long.Parse(s) };
        }
    }

    static long GetValue(Argument arg, long[,] registers, int progId)
    {
        if (arg.IsReg)
        {
            return registers[progId, arg.Val];
        }
        else
        {
            return arg.Val;
        }
    }
}
