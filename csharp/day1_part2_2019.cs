
using System;
using System.IO;

class Program
{
    static int ProcessLine(string line)
    {
        return int.Parse(line.Trim());
    }

    static int GetTotal(int[] masses)
    {
        int total = 0;
        foreach (int mass in masses)
        {
            total += CalcFuelMass(mass);
        }
        return total;
    }

    static int CalcFuelMass(int mass)
    {
        int fuel = (int)(Math.Floor((double)mass / 3) - 2);
        if (fuel <= 0)
        {
            return 0;
        }
        return fuel + CalcFuelMass(fuel);
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] masses = new int[lines.Length];
        for (int i = 0; i < lines.Length; i++)
        {
            masses[i] = ProcessLine(lines[i]);
        }
        int total = GetTotal(masses);
        Console.WriteLine(total);
    }
}
