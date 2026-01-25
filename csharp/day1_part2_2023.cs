
using System;
using System.IO;
using System.Linq;

public class Program
{
    private static readonly string[] digits = { "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };

    public static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("File not found");
            return;
        }

        int sum = File.ReadAllLines("input.txt")
            .Select(line => FindFirstAndLastDigit(line))
            .Sum();

        Console.WriteLine(sum);
    }

    private static int FindFirstAndLastDigit(string line)
    {
        int firstDigit = 0, lastDigit = 0;
        for (int i = 0; i < line.Length; i++)
        {
            if (char.IsDigit(line[i]))
            {
                int digit = line[i] - '0';
                if (firstDigit == 0) firstDigit = digit;
                lastDigit = digit;
            }
            else
            {
                for (int j = 0; j < digits.Length; j++)
                {
                    if (line.IndexOf(digits[j], i, StringComparison.Ordinal) == i)
                    {
                        if (firstDigit == 0) firstDigit = j;
                        lastDigit = j;
                        break;
                    }
                }
            }
        }

        return 10 * firstDigit + lastDigit;
    }
}
