
using System;
using System.IO;
using System.Linq;

class Program
{
    static int[] ParseStringToInts(string numbersLine)
    {
        return numbersLine.Split(' ').Select(int.Parse).ToArray();
    }

    static bool AllZeros(int[] nums)
    {
        return nums.All(num => num == 0);
    }

    static int[] CalculateExtrapolation(int[] history)
    {
        int[] extrapolations = new int[history.Length - 1];
        for (int i = 1; i < history.Length; i++)
        {
            extrapolations[i - 1] = history[i] - history[i - 1];
        }
        return extrapolations;
    }

    static int[][] CalculateExtrapolations(int[] history)
    {
        int[][] extrapolationsSeries = new int[1][];
        extrapolationsSeries[0] = history;

        for (int i = 1; i < history.Length; i++)
        {
            int[] previousExtrapolations = extrapolationsSeries[i - 1];
            if (AllZeros(previousExtrapolations))
            {
                return extrapolationsSeries;
            }

            int[] extrapolations = CalculateExtrapolation(previousExtrapolations);
            Array.Resize(ref extrapolationsSeries, extrapolationsSeries.Length + 1);
            extrapolationsSeries[i] = extrapolations;
        }

        return extrapolationsSeries;
    }

    static int Solve(string[] input)
    {
        int res = 0;

        foreach (var line in input)
        {
            int[] history = ParseStringToInts(line);
            int[][] extrapolationsSeries = CalculateExtrapolations(history);

            int futurePrediction = 0;
            for (int i = extrapolationsSeries.Length - 1; i > -1; i--)
            {
                futurePrediction = extrapolationsSeries[i][extrapolationsSeries[i].Length - 1] + futurePrediction;
            }

            res += futurePrediction;
        }

        return res;
    }

    static string[] ReadFile(string fileName)
    {
        return File.ReadAllLines(fileName);
    }

    static void Main()
    {
        string[] input = ReadFile("input.txt");
        Console.WriteLine(Solve(input));
    }
}
