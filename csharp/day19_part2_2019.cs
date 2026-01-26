
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;

public class VM
{
    private readonly Dictionary<long, long> _code = new Dictionary<long, long>();
    private long _ip;
    private readonly BlockingCollection<long> _input = new BlockingCollection<long>();
    private readonly BlockingCollection<long> _output = new BlockingCollection<long>();
    private long _relativeBase;

    public VM(string filename)
    {
        Load(filename);
    }

    private void Load(string filename)
    {
        var parts = File.ReadAllText(filename).Trim().Split(',');
        for (var i = 0L; i < parts.Length; i++)
        {
            _code[i] = long.Parse(parts[i]);
        }
        _ip = 0;
        _relativeBase = 0;
    }

    public void Run()
    {
        while (true)
        {
            var instruction = _code.ContainsKey(_ip) ? _code[_ip] : 0;
            var opcode = (int)(instruction % 100);

            switch (opcode)
            {
                case 1:
                    {
                        var params1 = GetParamsAddresses(_ip, instruction, 3);
                        _code[params1[2]] = GetValue(params1[0]) + GetValue(params1[1]);
                        _ip += 4;
                        break;
                    }
                case 2:
                    {
                        var params2 = GetParamsAddresses(_ip, instruction, 3);
                        _code[params2[2]] = GetValue(params2[0]) * GetValue(params2[1]);
                        _ip += 4;
                        break;
                    }
                case 3:
                    {
                        var params3 = GetParamsAddresses(_ip, instruction, 1);
                        _code[params3[0]] = _input.Take();
                        _ip += 2;
                        break;
                    }
                case 4:
                    {
                        var params4 = GetParamsAddresses(_ip, instruction, 1);
                        _output.Add(GetValue(params4[0]));
                        _ip += 2;
                        break;
                    }
                case 5:
                    {
                        var params5 = GetParamsAddresses(_ip, instruction, 2);
                        if (GetValue(params5[0]) != 0)
                        {
                            _ip = GetValue(params5[1]);
                        }
                        else
                        {
                            _ip += 3;
                        }
                        break;
                    }
                case 6:
                    {
                        var params6 = GetParamsAddresses(_ip, instruction, 2);
                        if (GetValue(params6[0]) == 0)
                        {
                            _ip = GetValue(params6[1]);
                        }
                        else
                        {
                            _ip += 3;
                        }
                        break;
                    }
                case 7:
                    {
                        var params7 = GetParamsAddresses(_ip, instruction, 3);
                        _code[params7[2]] = GetValue(params7[0]) < GetValue(params7[1]) ? 1 : 0;
                        _ip += 4;
                        break;
                    }
                case 8:
                    {
                        var params8 = GetParamsAddresses(_ip, instruction, 3);
                        _code[params8[2]] = GetValue(params8[0]) == GetValue(params8[1]) ? 1 : 0;
                        _ip += 4;
                        break;
                    }
                case 9:
                    {
                        var params9 = GetParamsAddresses(_ip, instruction, 1);
                        _relativeBase += GetValue(params9[0]);
                        _ip += 2;
                        break;
                    }
                case 99:
                    return;
                default:
                    throw new ArgumentException("Invalid opcode: " + opcode);
            }
        }
    }

    private long[] GetParamsAddresses(long pos, long instruction, int arity)
    {
        var modes = GetModes(instruction, arity);
        var addresses = new long[arity];
        for (var i = 0; i < arity; i++)
        {
            addresses[i] = GetParamAddress(pos + i + 1, modes[i]);
        }
        return addresses;
    }

    private long GetParamAddress(long pos, int mode)
    {
        switch (mode)
        {
            case 0:
                return _code.ContainsKey(pos) ? _code[pos] : 0;
            case 1:
                return pos;
            case 2:
                return _relativeBase + (_code.ContainsKey(pos) ? _code[pos] : 0);
            default:
                throw new ArgumentException("Invalid mode: " + mode);
        }
    }

    private int[] GetModes(long instruction, int arity)
    {
        var modeSection = (int)(instruction / 100);
        var modes = new int[arity];
        for (var i = 0; i < arity; i++)
        {
            modes[i] = modeSection / (int)Math.Pow(10, i) % 10;
        }
        return modes;
    }

    private long GetValue(long address)
    {
        return _code.ContainsKey(address) ? _code[address] : 0;
    }

    public void AddInput(long value)
    {
        _input.Add(value);
    }

    public long GetOutput()
    {
        return _output.Take();
    }
}

public class Program
{
    private static bool Beam(int x, int y)
    {
        var vm = new VM("input.txt");
        var thread = new Thread(vm.Run);
        thread.Start();
        vm.AddInput(x);
        vm.AddInput(y);
        var result = vm.GetOutput();
        thread.Join();
        return result == 1;
    }

    public static void Main(string[] args)
    {
        var y = 20;
        var x = 0;

        while (true)
        {
            if (!Beam(x, y))
            {
                x++;
                continue;
            }

            if (!Beam(x + 99, y))
            {
                y++;
                continue;
            }

            if (!Beam(x, y + 99))
            {
                x++;
                continue;
            }

            Console.WriteLine(x * 10000 + y);
            return;
        }
    }
}
