
using System;
using System.IO;
using System.Linq;

public class IntcodeComputer
{
    private long[] _memory;
    private long _ip;
    private long _relativeBase;

    public IntcodeComputer(long[] memory)
    {
        _memory = memory;
        _ip = 0;
        _relativeBase = 0;
    }

    private long GetParam(int offset, long modes)
    {
        int mode = (int)((modes / (long)Math.Pow(10, offset - 1)) % 10);
        long param = _memory[_ip + offset];

        switch (mode)
        {
            case 0:
                if (param < 0)
                    throw new InvalidOperationException($"Invalid memory access with mode 0 at pos:{_ip} param:{param}");
                EnsureMemorySize(param);
                return _memory[param];
            case 1:
                return param;
            case 2:
                if (_relativeBase + param < 0)
                    throw new InvalidOperationException($"Invalid memory access with mode 2 at pos:{_ip} param:{param} relativeBase:{_relativeBase}");
                EnsureMemorySize(_relativeBase + param);
                return _memory[_relativeBase + param];
            default:
                throw new InvalidOperationException($"Unknown parameter mode: {mode}");
        }
    }

    private void SetParam(int offset, long modes, long value)
    {
        int mode = (int)((modes / (long)Math.Pow(10, offset - 1)) % 10);
        long param = _memory[_ip + offset];

        switch (mode)
        {
            case 0:
                if (param < 0)
                    throw new InvalidOperationException($"Invalid memory access with mode 0 at pos:{_ip} param:{param}");
                EnsureMemorySize(param);
                _memory[param] = value;
                break;
            case 2:
                if (_relativeBase + param < 0)
                    throw new InvalidOperationException($"Invalid memory access with mode 2 at pos:{_ip} param:{param} relativeBase:{_relativeBase}");
                EnsureMemorySize(_relativeBase + param);
                _memory[_relativeBase + param] = value;
                break;
            default:
                throw new InvalidOperationException($"Unknown parameter mode: {mode}");
        }
    }

    private void EnsureMemorySize(long index)
    {
        if (index >= _memory.Length)
        {
            long newSize = Math.Max(_memory.Length * 2, index + 1);
            Array.Resize(ref _memory, (int)newSize);
        }
    }

    public long Run()
    {
        long output = 0;

        while (true)
        {
            long opcode = _memory[_ip] % 100;
            long modes = _memory[_ip] / 100;

            switch (opcode)
            {
                case 1:
                    SetParam(3, modes, GetParam(1, modes) + GetParam(2, modes));
                    _ip += 4;
                    break;
                case 2:
                    SetParam(3, modes, GetParam(1, modes) * GetParam(2, modes));
                    _ip += 4;
                    break;
                case 3:
                    SetParam(1, modes, 1);
                    _ip += 2;
                    break;
                case 4:
                    output = GetParam(1, modes);
                    _ip += 2;
                    break;
                case 5:
                    if (GetParam(1, modes) != 0)
                        _ip = GetParam(2, modes);
                    else
                        _ip += 3;
                    break;
                case 6:
                    if (GetParam(1, modes) == 0)
                        _ip = GetParam(2, modes);
                    else
                        _ip += 3;
                    break;
                case 7:
                    SetParam(3, modes, GetParam(1, modes) < GetParam(2, modes) ? 1 : 0);
                    _ip += 4;
                    break;
                case 8:
                    SetParam(3, modes, GetParam(1, modes) == GetParam(2, modes) ? 1 : 0);
                    _ip += 4;
                    break;
                case 9:
                    _relativeBase += GetParam(1, modes);
                    _ip += 2;
                    break;
                case 99:
                    return output;
                default:
                    throw new InvalidOperationException($"Unknown opcode: {opcode}");
            }
        }
    }
}

public class Program
{
    public static void Main()
    {
        try
        {
            string input = File.ReadAllText("input.txt");
            long[] memory = input.Split(',').Select(long.Parse).ToArray();
            IntcodeComputer computer = new IntcodeComputer(memory);
            long result = computer.Run();
            Console.WriteLine(result);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine(ex.Message);
        }
    }
}
