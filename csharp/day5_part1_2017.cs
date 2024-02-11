
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] offsets = Array.ConvertAll(lines, int.Parse);

        int index = 0;
        int steps = 0;

        while (index >= 0 && index < offsets.Length)
        {
            int jump = offsets[index];
            offsets[index]++;
            index += jump;
            steps++;
        }

        Console.WriteLine(steps);
    }
}
