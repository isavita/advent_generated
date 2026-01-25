
using System;
using System.IO;
using System.Linq;

class Program
{
    static int Get(int[] p, int pos, int mode) => mode == 0 ? p[p[pos]] : p[pos];

    static void Main()
    {
        var program = File.ReadAllText("input.txt")
            .Split(',', StringSplitOptions.RemoveEmptyEntries)
            .Select(int.Parse)
            .ToArray();

        int input = 5, i = 0;
        while (true)
        {
            int opcode = program[i] % 100;
            int modes = program[i] / 100;
            int m1 = modes % 10;
            int m2 = (modes / 10) % 10;

            switch (opcode)
            {
                case 1:
                    program[program[i + 3]] = Get(program, i + 1, m1) + Get(program, i + 2, m2);
                    i += 4;
                    break;
                case 2:
                    program[program[i + 3]] = Get(program, i + 1, m1) * Get(program, i + 2, m2);
                    i += 4;
                    break;
                case 3:
                    program[program[i + 1]] = input;
                    i += 2;
                    break;
                case 4:
                    Console.WriteLine(Get(program, i + 1, m1));
                    i += 2;
                    break;
                case 5:
                    i = Get(program, i + 1, m1) != 0 ? Get(program, i + 2, m2) : i + 3;
                    break;
                case 6:
                    i = Get(program, i + 1, m1) == 0 ? Get(program, i + 2, m2) : i + 3;
                    break;
                case 7:
                    program[program[i + 3]] = Get(program, i + 1, m1) < Get(program, i + 2, m2) ? 1 : 0;
                    i += 4;
                    break;
                case 8:
                    program[program[i + 3]] = Get(program, i + 1, m1) == Get(program, i + 2, m2) ? 1 : 0;
                    i += 4;
                    break;
                case 99:
                    return;
                default:
                    return;
            }
        }
    }
}
