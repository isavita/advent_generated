
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] freqChanges = File.ReadAllLines("input.txt");
        int freq = 0;
        foreach (var change in freqChanges)
        {
            freq += ParseChange(change);
        }
        Console.WriteLine(freq);
    }

    static int ParseChange(string change)
    {
        var (sign, num) = ParseSignNum(change);
        return sign * num;
    }

    static (int, int) ParseSignNum(string change)
    {
        int sign = 1;
        if (change[0] == '-')
        {
            sign = -1;
            change = change.Substring(1);
        }
        int num = int.Parse(change);
        return (sign, num);
    }
}
