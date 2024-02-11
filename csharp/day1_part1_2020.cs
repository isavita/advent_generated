
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        int[] numbers = File.ReadAllLines("input.txt")
                            .Where(line => !string.IsNullOrEmpty(line))
                            .Select(int.Parse)
                            .ToArray();

        for (int i = 0; i < numbers.Length - 1; i++)
        {
            for (int j = i + 1; j < numbers.Length; j++)
            {
                if (numbers[i] + numbers[j] == 2020)
                {
                    Console.WriteLine(numbers[i] * numbers[j]);
                    return;
                }
            }
        }
    }
}
