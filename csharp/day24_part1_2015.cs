
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] packages = Array.ConvertAll(lines, int.Parse);
        int totalWeight = packages.Sum();

        int targetWeight = totalWeight / 3;
        long bestQE = long.MaxValue;
        int bestLength = int.MaxValue;

        FindBestQE(packages, 0, 1, 0, 0, targetWeight, ref bestQE, ref bestLength);

        Console.WriteLine(bestQE);
    }

    static void FindBestQE(int[] packages, int index, long currentQE, int currentLength, int currentWeight, int targetWeight, ref long bestQE, ref int bestLength)
    {
        if (currentWeight == targetWeight)
        {
            if (currentLength < bestLength || (currentLength == bestLength && currentQE < bestQE))
            {
                bestLength = currentLength;
                bestQE = currentQE;
            }
            return;
        }

        if (index == packages.Length || currentLength >= bestLength || currentWeight > targetWeight)
        {
            return;
        }

        FindBestQE(packages, index + 1, currentQE * packages[index], currentLength + 1, currentWeight + packages[index], targetWeight, ref bestQE, ref bestLength);
        FindBestQE(packages, index + 1, currentQE, currentLength, currentWeight, targetWeight, ref bestQE, ref bestLength);
    }
}
