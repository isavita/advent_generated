
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[][] numbers = lines
            .Where(line => !string.IsNullOrWhiteSpace(line))
            .Select(line => line.Trim().Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)
                .Select(str => int.TryParse(str, out int num) ? num : 0)
                .ToArray())
            .ToArray();

        int validTriangles = 0;
        for (int i = 0; i < numbers[0].Length; i++)
        {
            for (int j = 0; j < numbers.Length; j += 3)
            {
                if (j + 2 < numbers.Length && IsValidTriangle(numbers[j][i], numbers[j + 1][i], numbers[j + 2][i]))
                {
                    validTriangles++;
                }
            }
        }

        Console.WriteLine(validTriangles);
    }

    static bool IsValidTriangle(int a, int b, int c)
    {
        return a + b > c && a + c > b && b + c > a;
    }
}
