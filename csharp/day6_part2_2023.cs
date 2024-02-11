
using System;
using System.IO;

class Program
{
    static int CalculateWaysToWinLongRace(long time, long record)
    {
        int waysToWin = 0;
        for (long holdTime = 1; holdTime < time; holdTime++)
        {
            long travelTime = time - holdTime;
            long distance = holdTime * travelTime;
            if (distance > record)
            {
                waysToWin++;
            }
        }
        return waysToWin;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        long time = 0, distance = 0;

        foreach (string line in lines)
        {
            if (string.IsNullOrEmpty(line))
            {
                continue;
            }
            string[] parts = line.Split(':');
            string cleanLine = parts[1].Replace(" ", "");
            if (time == 0)
            {
                time = long.Parse(cleanLine);
            }
            else
            {
                distance = long.Parse(cleanLine);
            }
        }

        int waysToWin = CalculateWaysToWinLongRace(time, distance);

        Console.WriteLine(waysToWin);
    }
}
