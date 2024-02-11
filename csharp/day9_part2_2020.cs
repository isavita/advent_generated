
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        long invalidNumber = 14360655;
        List<long> numbers = new List<long>();

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                numbers.Add(long.Parse(line));
            }
        }

        for (int i = 0; i < numbers.Count; i++)
        {
            long sum = numbers[i];
            long min = numbers[i];
            long max = numbers[i];
            for (int j = i + 1; j < numbers.Count; j++)
            {
                sum += numbers[j];
                if (numbers[j] < min)
                {
                    min = numbers[j];
                }
                if (numbers[j] > max)
                {
                    max = numbers[j];
                }
                if (sum == invalidNumber)
                {
                    Console.WriteLine(min + max);
                    return;
                }
                else if (sum > invalidNumber)
                {
                    break;
                }
            }
        }
    }
}
