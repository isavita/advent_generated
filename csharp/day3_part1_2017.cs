
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int target = int.Parse(input);

        int sideLength = (int)Math.Ceiling(Math.Sqrt(target));
        if (sideLength % 2 == 0)
        {
            sideLength++;
        }

        int maxValue = sideLength * sideLength;
        int stepsFromEdge = (sideLength - 1) / 2;
        int distanceToMiddle = 0;

        for (int i = 0; i < 4; i++)
        {
            int middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i;
            int distance = Math.Abs(target - middlePoint);
            if (distance < distanceToMiddle || i == 0)
            {
                distanceToMiddle = distance;
            }
        }

        int manhattanDistance = stepsFromEdge + distanceToMiddle;

        Console.WriteLine(manhattanDistance);
    }
}
