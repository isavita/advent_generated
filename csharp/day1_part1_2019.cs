
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        double total = 0;

        foreach (string line in lines)
        {
            int mass = int.Parse(line.Trim());
            total += Math.Floor((double)mass / 3) - 2;
        }

        Console.WriteLine(total);
    }
}
