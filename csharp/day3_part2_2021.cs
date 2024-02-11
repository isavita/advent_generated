
using System;
using System.IO;
using System.Linq;

class Program
{
    static string FilterValues(string[] values, Func<int, int, char> criteria)
    {
        for (int i = 0; i < values[0].Length; i++)
        {
            int zeros = 0, ones = 0;
            foreach (var val in values)
            {
                if (val[i] == '0')
                {
                    zeros++;
                }
                else
                {
                    ones++;
                }
            }
            char keep = criteria(zeros, ones);
            values = FilterByBit(values, i, keep);
            if (values.Length == 1)
            {
                break;
            }
        }
        return values[0];
    }

    static string[] FilterByBit(string[] values, int bitIndex, char keep)
    {
        return values.Where(val => val[bitIndex] == keep).ToArray();
    }

    static void Main()
    {
        string[] values = File.ReadAllLines("input.txt");

        string oxygenGeneratorRating = FilterValues(values, (zeros, ones) => zeros > ones ? '0' : '1');
        long oxygenGeneratorRatingInt = Convert.ToInt64(oxygenGeneratorRating, 2);

        string co2ScrubberRating = FilterValues(values, (zeros, ones) => zeros <= ones ? '0' : '1');
        long co2ScrubberRatingInt = Convert.ToInt64(co2ScrubberRating, 2);

        Console.WriteLine(oxygenGeneratorRatingInt * co2ScrubberRatingInt);
    }
}
