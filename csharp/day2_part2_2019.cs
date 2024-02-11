
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        string[] strs = input.Trim().Split(',');
        int[] original = new int[strs.Length];
        for (int i = 0; i < strs.Length; i++)
        {
            original[i] = int.Parse(strs[i]);
        }

        for (int noun = 0; noun <= 99; noun++)
        {
            for (int verb = 0; verb <= 99; verb++)
            {
                int[] memory = new int[original.Length];
                original.CopyTo(memory, 0);
                memory[1] = noun;
                memory[2] = verb;
                if (Execute(memory) == 19690720)
                {
                    Console.WriteLine(100 * noun + verb);
                    return;
                }
            }
        }
    }

    static int Execute(int[] memory)
    {
        for (int i = 0; i < memory.Length; i += 4)
        {
            switch (memory[i])
            {
                case 1:
                    memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]];
                    break;
                case 2:
                    memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]];
                    break;
                case 99:
                    return memory[0];
            }
        }
        return memory[0];
    }
}
