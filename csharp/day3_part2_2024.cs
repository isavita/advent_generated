
using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("Error: input.txt not found.");
            return;
        }

        string input = File.ReadAllText("input.txt");
        long totalSum = 0;
        bool enabled = true;

        int pos = 0;
        while (pos < input.Length)
        {
            if (input.Substring(pos).StartsWith("mul("))
            {
                if (enabled)
                {
                    pos += 4;
                    int num1 = 0;
                    while (pos < input.Length && char.IsDigit(input[pos]))
                    {
                        num1 = num1 * 10 + (input[pos] - '0');
                        pos++;
                    }

                    if (pos >= input.Length || input[pos] != ',')
                    {
                        continue;
                    }
                    pos++;
                    int num2 = 0;

                    while (pos < input.Length && char.IsDigit(input[pos]))
                    {
                        num2 = num2 * 10 + (input[pos] - '0');
                        pos++;
                    }
                    if (pos >= input.Length || input[pos] != ')')
                    {
                        continue;
                    }
                    totalSum += num1 * num2;
                }
                pos++;
            }
            else if (input.Substring(pos).StartsWith("do()"))
            {
                enabled = true;
                pos += 4;
            }
            else if (input.Substring(pos).StartsWith("don't()"))
            {
                enabled = false;
                pos += 7;
            }
            else
            {
                pos++;
            }
        }

        Console.WriteLine(totalSum);
    }
}
