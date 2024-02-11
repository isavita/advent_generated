
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    const int PreambleLength = 25;

    static void Main()
    {
        List<long> numbers = new List<long>();
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                numbers.Add(long.Parse(line));
            }
        }

        for (int i = PreambleLength; i < numbers.Count; i++)
        {
            if (!IsValid(numbers[i], numbers.GetRange(i - PreambleLength, PreambleLength)))
            {
                Console.WriteLine(numbers[i]);
                break;
            }
        }
    }

    static bool IsValid(long number, List<long> previousNumbers)
    {
        HashSet<long> seen = new HashSet<long>();
        foreach (long n in previousNumbers)
        {
            if (seen.Contains(number - n))
            {
                return true;
            }
            seen.Add(n);
        }
        return false;
    }
}
