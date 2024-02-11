
using System;
using System.IO;

class Program
{
    static void Main()
    {
        int totalElves = ReadInput("input.txt");
        int winner = FindWinningElf(totalElves);
        Console.WriteLine(winner);
    }

    static int ReadInput(string filename)
    {
        using (StreamReader sr = new StreamReader(filename))
        {
            int totalElves = int.Parse(sr.ReadLine());
            return totalElves;
        }
    }

    static int FindWinningElf(int totalElves)
    {
        int highestPowerOfTwo = 1;
        while (highestPowerOfTwo * 2 <= totalElves)
        {
            highestPowerOfTwo *= 2;
        }
        return (totalElves - highestPowerOfTwo) * 2 + 1;
    }
}
