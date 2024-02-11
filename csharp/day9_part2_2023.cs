
using System;
using System.IO;
using System.Linq;

class Program
{
    static int[][] ParseInput(string[] input)
    {
        return input.Select(line => line.Split(' ').Select(int.Parse).ToArray()).ToArray();
    }

    static bool AllZeros(int[] nums)
    {
        return nums.All(num => num == 0);
    }

    static int[] CalculateExtrapolation(int[] history)
    {
        return history.Skip(1).Select((num, index) => num - history[index]).ToArray();
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
            extrapolationsSeries = extrapolationsSeries.Append(extrapolations).ToArray();
        }

        return extrapolationsSeries;
    }

    static int Solve(string[] input)
    {
        int[][] histories = ParseInput(input);
        int res = 0;

        foreach (int[] history in histories)
        {
            int[][] extrapolationsSeries = CalculateExtrapolations(history);

            int pastPrediction = 0;
            for (int i = extrapolationsSeries.Length - 1; i > -1; i--)
            {
                pastPrediction = extrapolationsSeries[i][0] - pastPrediction;
            }

            res += pastPrediction;
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
