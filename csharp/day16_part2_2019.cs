
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();

        int[] repeatedInput = RepeatInput(input, 10000);

        int offset = int.Parse(input.Substring(0, 7));

        for (int phase = 0; phase < 100; phase++)
        {
            int sum = 0;
            for (int i = repeatedInput.Length - 1; i >= offset; i--)
            {
                sum += repeatedInput[i];
                repeatedInput[i] = sum % 10;
            }
        }

        Console.WriteLine(string.Join("", repeatedInput.Skip(offset).Take(8)));
    }

    static int[] RepeatInput(string input, int times)
    {
        return Enumerable.Range(0, times)
            .SelectMany(_ => input.Select(c => int.Parse(c.ToString())))
            .ToArray();
    }
}
