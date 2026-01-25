
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("File not found.");
            return;
        }

        var lines = File.ReadAllLines("input.txt");
        if (lines.Length < 2)
        {
            Console.WriteLine("Invalid input file.");
            return;
        }

        var times = lines[0].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries).Skip(1).Select(long.Parse).ToArray();
        var distances = lines[1].Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries).Skip(1).Select(long.Parse).ToArray();

        long totalWays = 1;
        for (int i = 0; i < times.Length; i++)
        {
            totalWays *= CalculateWaysToWin(times[i], distances[i]);
        }

        Console.WriteLine(totalWays);
    }

    static long CalculateWaysToWin(long time, long record)
    {
        long low = 1, high = time - 1;
        while (low <= high)
        {
            long mid = low + (high - low) / 2;
            long dist = mid * (time - mid);
            if (dist > record)
            {
                high = mid - 1;
            }
            else
            {
                low = mid + 1;
            }
        }
        return time - 2 * low + 1;
    }
}
