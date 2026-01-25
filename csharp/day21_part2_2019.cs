
using System;
using System.IO;
using System.Linq;

public class IntcodeVM
{
    private const int MEM_SIZE = 16384;
    private const int INPUT_BUFFER_SIZE = 256;
    private const int OUTPUT_BUFFER_SIZE = 256;

    private long[] code;
    private long ip;
    private long relativeBase;
    private long[] inputBuffer;
    private int inputHead;
    private int inputTail;
    private long[] outputBuffer;
    private int outputHead;
    private int outputTail;
    private bool halted;

    public IntcodeVM()
    {
        code = new long[MEM_SIZE];
        inputBuffer = new long[INPUT_BUFFER_SIZE];
        outputBuffer = new long[OUTPUT_BUFFER_SIZE];
        Reset();
    }

    public void Reset()
    {
        Array.Clear(code, 0, code.Length);
        ip = 0;
        relativeBase = 0;
        inputHead = 0;
        inputTail = 0;
        outputHead = 0;
        outputTail = 0;
        halted = false;
    }

    public void Load(string filename)
    {
        var data = File.ReadAllText(filename).Trim();
        var values = data.Split(',').Select(long.Parse).ToArray();
        Array.Copy(values, code, values.Length);
    }

    public void Run()
    {
        while (!halted)
        {
            var instruction = code[ip];
            var opcode = instruction % 100;
            var mode1 = (instruction / 100) % 10;
            var mode2 = (instruction / 1000) % 10;
            var mode3 = (instruction / 10000) % 10;

            long param1, param2, param3;
            switch (opcode)
            {
                case 1:
                    param1 = GetValue(ip + 1, mode1);
                    param2 = GetValue(ip + 2, mode2);
                    param3 = GetParamAddress(ip + 3, mode3);
                    code[param3] = param1 + param2;
                    ip += 4;
                    break;
                case 2:
                    param1 = GetValue(ip + 1, mode1);
                    param2 = GetValue(ip + 2, mode2);
                    param3 = GetParamAddress(ip + 3, mode3);
                    code[param3] = param1 * param2;
                    ip += 4;
                    break;
                case 3:
                    param1 = GetParamAddress(ip + 1, mode1);
                    if (inputHead == inputTail)
                    {
                        return;
                    }
                    code[param1] = inputBuffer[inputHead++];
                    if (inputHead >= INPUT_BUFFER_SIZE)
                    {
                        inputHead = 0;
                    }
                    ip += 2;
                    break;
                case 4:
                    param1 = GetValue(ip + 1, mode1);
                    outputBuffer[outputTail++] = param1;
                    if (outputTail >= OUTPUT_BUFFER_SIZE)
                    {
                        outputTail = 0;
                    }
                    ip += 2;
                    break;
                case 5:
                    param1 = GetValue(ip + 1, mode1);
                    param2 = GetValue(ip + 2, mode2);
                    if (param1 != 0)
                    {
                        ip = param2;
                    }
                    else
                    {
                        ip += 3;
                    }
                    break;
                case 6:
                    param1 = GetValue(ip + 1, mode1);
                    param2 = GetValue(ip + 2, mode2);
                    if (param1 == 0)
                    {
                        ip = param2;
                    }
                    else
                    {
                        ip += 3;
                    }
                    break;
                case 7:
                    param1 = GetValue(ip + 1, mode1);
                    param2 = GetValue(ip + 2, mode2);
                    param3 = GetParamAddress(ip + 3, mode3);
                    code[param3] = param1 < param2 ? 1 : 0;
                    ip += 4;
                    break;
                case 8:
                    param1 = GetValue(ip + 1, mode1);
                    param2 = GetValue(ip + 2, mode2);
                    param3 = GetParamAddress(ip + 3, mode3);
                    code[param3] = param1 == param2 ? 1 : 0;
                    ip += 4;
                    break;
                case 9:
                    param1 = GetValue(ip + 1, mode1);
                    relativeBase += param1;
                    ip += 2;
                    break;
                case 99:
                    halted = true;
                    break;
                default:
                    throw new Exception($"Invalid opcode: {opcode}");
            }
        }
    }

    public void SendString(string s)
    {
        foreach (var c in s)
        {
            inputBuffer[inputTail++] = c;
            if (inputTail >= INPUT_BUFFER_SIZE)
            {
                inputTail = 0;
            }
        }
        inputBuffer[inputTail++] = '\n';
        if (inputTail >= INPUT_BUFFER_SIZE)
        {
            inputTail = 0;
        }
    }

    public void ReadOutput()
    {
        while (outputHead != outputTail)
        {
            var c = outputBuffer[outputHead++];
            if (outputHead >= OUTPUT_BUFFER_SIZE)
            {
                outputHead = 0;
            }
            if (c > 127)
            {
                Console.WriteLine(c);
                return;
            }
        }
    }

    private long GetParamAddress(long pos, long mode)
    {
        switch (mode)
        {
            case 0:
                return code[pos];
            case 1:
                return pos;
            case 2:
                return relativeBase + code[pos];
            default:
                throw new Exception($"Invalid parameter mode: {mode}");
        }
    }

    private long GetValue(long pos, long mode)
    {
        var address = GetParamAddress(pos, mode);
        return code[address];
    }
}

class Program
{
    static void Main(string[] args)
    {
        var vm = new IntcodeVM();
        vm.Load("input.txt");

        string[] instructions = new string[]
        {
            "NOT A J",
            "NOT B T",
            "OR T J",
            "NOT C T",
            "OR T J",
            "AND D J",
            "NOT A T",
            "AND A T",
            "OR E T",
            "OR H T",
            "AND T J",
            "RUN"
        };

        foreach (var instruction in instructions)
        {
            vm.SendString(instruction);
        }

        vm.Run();
        vm.ReadOutput();
    }
}
