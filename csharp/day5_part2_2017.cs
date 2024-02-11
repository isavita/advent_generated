
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] offsets = lines.Select(int.Parse).ToArray();

        int index = 0;
        int steps = 0;

        while (index >= 0 && index < offsets.Length)
        {
            int jump = offsets[index];

            if (jump >= 3)
            {
                offsets[index]--;
            }
            else
            {
                offsets[index]++;
            }

            index += jump;
            steps++;
        }

        Console.WriteLine(steps);
    }
}
