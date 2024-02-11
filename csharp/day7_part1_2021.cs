
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
                fuel += CalculateFuel(pos, i);
            }
            minFuel = Math.Min(minFuel, fuel);
        }

        Console.WriteLine(minFuel);
    }

    static int CalculateFuel(int currentPosition, int newPosition)
    {
        return Math.Abs(currentPosition - newPosition);
    }
}
