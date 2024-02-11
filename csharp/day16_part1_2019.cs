
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();

        int[] digits = input.Select(c => int.Parse(c.ToString())).ToArray();

        for (int phase = 0; phase < 100; phase++)
        {
            digits = ApplyFFT(digits);
        }

        for (int i = 0; i < 8; i++)
        {
            Console.Write(digits[i]);
        }
        Console.WriteLine();
    }

    static int[] ApplyFFT(int[] input)
    {
        int[] basePattern = { 0, 1, 0, -1 };
        int[] output = new int[input.Length];

        for (int i = 0; i < input.Length; i++)
        {
            int sum = 0;
            for (int j = 0; j < input.Length; j++)
            {
                int patternValue = basePattern[((j + 1) / (i + 1)) % basePattern.Length];
                sum += input[j] * patternValue;
            }
            output[i] = Math.Abs(sum % 10);
        }

        return output;
    }
}
