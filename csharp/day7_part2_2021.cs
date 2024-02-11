
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] positions = lines.SelectMany(line => line.Split(',')).Select(int.Parse).ToArray();
        Array.Sort(positions);

        int minFuel = int.MaxValue;
        for (int i = positions[0]; i <= positions[positions.Length - 1]; i++)
        {
            int fuel = 0;
            foreach (int pos in positions)
            {
                fuel += CalculateNewFuel(pos, i);
            }
            minFuel = Math.Min(minFuel, fuel);
        }

        Console.WriteLine(minFuel);
    }

    static int CalculateNewFuel(int currentPosition, int newPosition)
    {
        int diff = Math.Abs(currentPosition - newPosition);
        return (diff * (diff + 1)) / 2;
    }
}
