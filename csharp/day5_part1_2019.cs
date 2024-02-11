
using System;
using System.IO;
using System.Linq;

class Program
{
    static int GetMode(int instruction, int position)
    {
        return instruction / (int)Math.Pow(10, position + 1) % 10;
    }

    static int GetParam(int[] program, int pointer, int mode)
    {
        if (mode == 0)
        {
            return program[program[pointer]];
        }
        return program[pointer];
    }

    static int RunProgram(int[] program, int input)
    {
        int output = 0;
        for (int pointer = 0; pointer < program.Length;)
        {
            int instruction = program[pointer];
            int opcode = instruction % 100;

            switch (opcode)
            {
                case 1:
                case 2:
                    int param1 = GetParam(program, pointer + 1, GetMode(instruction, 1));
                    int param2 = GetParam(program, pointer + 2, GetMode(instruction, 2));
                    int result = opcode == 1 ? param1 + param2 : param1 * param2;
                    program[program[pointer + 3]] = result;
                    pointer += 4;
                    break;
                case 3:
                    program[program[pointer + 1]] = input;
                    pointer += 2;
                    break;
                case 4:
                    output = GetParam(program, pointer + 1, GetMode(instruction, 1));
                    pointer += 2;
                    break;
                case 99:
                    return output;
                default:
                    throw new Exception($"Unknown opcode: {opcode}");
            }
        }
        return output;
    }

    static void Main()
    {
        string[] strProgram = File.ReadAllText("input.txt").Trim().Split(',');
        int[] program = strProgram.Select(int.Parse).ToArray();
        Console.WriteLine(RunProgram(program, 1));
    }
}
