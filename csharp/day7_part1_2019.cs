
using System;
using System.IO;
using System.Linq;

public class Program
{
    private static int[] phases = { 0, 1, 2, 3, 4 };
    private static int maxOutput = 0;

    public static void Main(string[] args)
    {
        string[] program = File.ReadAllText("input.txt").Split(',');
        int[] memory = program.Select(int.Parse).ToArray();

        Permute(phases, 0, (int[])memory.Clone());

        Console.WriteLine("Max thruster signal: " + maxOutput);
    }

    private static void Permute(int[] phases, int start, int[] memory)
    {
        if (start == phases.Length)
        {
            int output = 0;
            for (int i = 0; i < phases.Length; i++)
            {
                int[] inputs = new int[] { phases[i], output };
                output = RunProgram((int[])memory.Clone(), inputs);
            }
            maxOutput = Math.Max(maxOutput, output);
        }
        else
        {
            for (int i = start; i < phases.Length; i++)
            {
                Swap(phases, start, i);
                Permute(phases, start + 1, memory);
                Swap(phases, start, i);
            }
        }
    }

    private static void Swap(int[] phases, int i, int j)
    {
        int temp = phases[i];
        phases[i] = phases[j];
        phases[j] = temp;
    }

    private static int RunProgram(int[] memory, int[] inputs)
    {
        int ip = 0;
        int inputIndex = 0;
        while (ip < memory.Length)
        {
            int opcode = memory[ip] % 100;
            int[] modes = GetModes(memory[ip] / 100);
            int[] @params = new int[3];
            for (int i = 0; i < 3; i++)
            {
                @params[i] = modes[i] == 0 ? memory[ip + i + 1] : ip + i + 1;
            }
            switch (opcode)
            {
                case 1:
                    memory[@params[2]] = memory[@params[0]] + memory[@params[1]];
                    ip += 4;
                    break;
                case 2:
                    memory[@params[2]] = memory[@params[0]] * memory[@params[1]];
                    ip += 4;
                    break;
                case 3:
                    memory[@params[0]] = inputs[inputIndex++];
                    ip += 2;
                    break;
                case 4:
                    ip += 2;
                    return memory[@params[0]];
                case 5:
                    ip = memory[@params[0]] != 0 ? memory[@params[1]] : ip + 3;
                    break;
                case 6:
                    ip = memory[@params[0]] == 0 ? memory[@params[1]] : ip + 3;
                    break;
                case 7:
                    memory[@params[2]] = memory[@params[0]] < memory[@params[1]] ? 1 : 0;
                    ip += 4;
                    break;
                case 8:
                    memory[@params[2]] = memory[@params[0]] == memory[@params[1]] ? 1 : 0;
                    ip += 4;
                    break;
                case 99:
                    return inputs[1];
            }
        }
        return 0;
    }

    private static int[] GetModes(int modes)
    {
        return new int[] { modes % 10, (modes / 10) % 10, (modes / 100) % 10 };
    }
}
