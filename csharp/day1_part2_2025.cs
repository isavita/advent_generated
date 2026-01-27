
using System;
using System.IO;

class Program
{
    static int FloorDiv(int a, int b)
    {
        int q = a / b;
        return a < 0 && a % b != 0 ? q - 1 : q;
    }

    static void Main()
    {
        const int dialSize = 100;
        int currentPos = 50;
        long totalZeroHits = 0;

        foreach (var rawLine in File.ReadLines("input.txt"))
        {
            var line = rawLine.Trim();
            if (line.Length == 0) continue;

            char direction = line[0];
            int amount = int.Parse(line.AsSpan(1));

            if (direction == 'R')
            {
                totalZeroHits += (currentPos + amount) / dialSize;
                currentPos = (currentPos + amount) % dialSize;
            }
            else // 'L'
            {
                totalZeroHits += FloorDiv(currentPos - 1, dialSize) - FloorDiv(currentPos - amount - 1, dialSize);
                currentPos = (currentPos - amount) % dialSize;
                if (currentPos < 0) currentPos += dialSize;
            }
        }

        Console.WriteLine($"The password is: {totalZeroHits}");
    }
}
